"""
prikol.py — логгер физики РА-3 в ZDSimulator.

Читает из процесса Launcher.exe ключевые переменные (контроллер, скорость,
реверсор, LOCSECTIONS[36]) и пишет в prikol.log с меткой времени.

Использование:
  python prikol.py            — обычное логирование
  python prikol.py -d         — диагностика: дамп памяти + свип LOCSECTIONS,
                                записывает в prikol.log и выходит

Требует Python 3.x, никаких внешних пакетов (только ctypes + Windows API).
Запускать от администратора, если игра тоже запущена от админа.
"""

import ctypes
import ctypes.wintypes as wt
import struct
import time
import sys
import os

from addresses import (
    ADDR_KONTROLLER,
    ADDR_LOCS_BASE_PTR,
    ADDR_REVERS_BASE_PTR,
    ADDR_SPEED_BASE_PTR,
    OFF_LOCS_36,
    OFF_REVERS,
    OFF_SPEED_KPH,
)

# ============================================================================
# Настройки
# ============================================================================
PROCESS_NAME   = "Launcher.exe"
LOG_FILE       = os.path.join(os.path.dirname(os.path.abspath(__file__)), "prikol.log")
TICK_HZ        = 10
LOG_ALL_TICKS  = False   # True = писать каждый тик, False = только при изменениях

# Адреса вынесены в addresses.py (см. import выше).

# ============================================================================
# Windows API
# ============================================================================
PROCESS_VM_READ           = 0x0010
PROCESS_QUERY_INFORMATION = 0x0400
TH32CS_SNAPPROCESS        = 0x00000002
TH32CS_SNAPMODULE         = 0x00000008
TH32CS_SNAPMODULE32       = 0x00000010
INVALID_HANDLE_VALUE      = -1

kernel32 = ctypes.windll.kernel32


class PROCESSENTRY32(ctypes.Structure):
    _fields_ = [
        ("dwSize",              wt.DWORD),
        ("cntUsage",            wt.DWORD),
        ("th32ProcessID",       wt.DWORD),
        ("th32DefaultHeapID",   ctypes.c_void_p),
        ("th32ModuleID",        wt.DWORD),
        ("cntThreads",          wt.DWORD),
        ("th32ParentProcessID", wt.DWORD),
        ("pcPriClassBase",      wt.LONG),
        ("dwFlags",             wt.DWORD),
        ("szExeFile",           ctypes.c_char * 260),
    ]


class MODULEENTRY32(ctypes.Structure):
    _fields_ = [
        ("dwSize",        wt.DWORD),
        ("th32ModuleID",  wt.DWORD),
        ("th32ProcessID", wt.DWORD),
        ("GlblcntUsage",  wt.DWORD),
        ("ProccntUsage",  wt.DWORD),
        ("modBaseAddr",   ctypes.c_void_p),
        ("modBaseSize",   wt.DWORD),
        ("hModule",       wt.HMODULE),
        ("szModule",      ctypes.c_char * 256),
        ("szExePath",     ctypes.c_char * 260),
    ]


def find_all_process_pids(name_bytes):
    """Возвращает список всех PID процессов с указанным именем."""
    result = []
    snap = kernel32.CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0)
    if snap == INVALID_HANDLE_VALUE:
        return result
    try:
        entry = PROCESSENTRY32()
        entry.dwSize = ctypes.sizeof(PROCESSENTRY32)
        if not kernel32.Process32First(snap, ctypes.byref(entry)):
            return result
        while True:
            if entry.szExeFile.lower() == name_bytes.lower():
                result.append(entry.th32ProcessID)
            if not kernel32.Process32Next(snap, ctypes.byref(entry)):
                break
        return result
    finally:
        kernel32.CloseHandle(snap)


def find_module_base(pid, name_bytes):
    snap = kernel32.CreateToolhelp32Snapshot(
        TH32CS_SNAPMODULE | TH32CS_SNAPMODULE32, pid
    )
    if snap == INVALID_HANDLE_VALUE:
        return None
    try:
        entry = MODULEENTRY32()
        entry.dwSize = ctypes.sizeof(MODULEENTRY32)
        if not kernel32.Module32First(snap, ctypes.byref(entry)):
            return None
        while True:
            if entry.szModule.lower() == name_bytes.lower():
                base = entry.modBaseAddr
                if base is None:
                    return 0
                return int(base)
            if not kernel32.Module32Next(snap, ctypes.byref(entry)):
                return None
    finally:
        kernel32.CloseHandle(snap)


def is_wow64(handle):
    """True если 32-битный процесс на 64-битной Windows."""
    IsWow64Process = kernel32.IsWow64Process
    IsWow64Process.argtypes = [wt.HANDLE, ctypes.POINTER(wt.BOOL)]
    IsWow64Process.restype = wt.BOOL
    flag = wt.BOOL(False)
    if IsWow64Process(handle, ctypes.byref(flag)):
        return bool(flag.value)
    return False


class Reader:
    def __init__(self, proc_name):
        name_bytes = proc_name.encode("ascii")
        pids = find_all_process_pids(name_bytes)
        if not pids:
            raise RuntimeError(f"Процесс {proc_name} не найден")

        print(f"[SCAN] Найдено процессов '{proc_name}': {len(pids)} — PIDs: {pids}")

        candidates = []
        for pid in pids:
            h = kernel32.OpenProcess(
                PROCESS_VM_READ | PROCESS_QUERY_INFORMATION, False, pid
            )
            if not h:
                print(f"[SCAN] PID={pid}: OpenProcess failed "
                      f"(err={kernel32.GetLastError()})")
                continue
            base = find_module_base(pid, name_bytes)
            wow64 = is_wow64(h)
            print(f"[SCAN] PID={pid}: base=0x{base or 0:X}, "
                  f"bitness={'32' if wow64 else '64'}")
            if base is None:
                kernel32.CloseHandle(h)
                continue
            candidates.append((pid, h, int(base), wow64))

        if not candidates:
            raise RuntimeError("Ни один процесс не дал модуль")

        # Предпочитаем 32-битный с base ≤ 4GB (это ZDSimulator Launcher.exe)
        chosen = None
        for pid, h, base, wow64 in candidates:
            if wow64 and base < 0x100000000:
                chosen = (pid, h, base, wow64)
                break

        if chosen is None:
            # Fallback: любой с base ≤ 4GB
            for pid, h, base, wow64 in candidates:
                if base < 0x100000000:
                    chosen = (pid, h, base, wow64)
                    break

        if chosen is None:
            # Последний шанс — берём первый и предупреждаем
            chosen = candidates[0]
            print("[WARN] Не нашёл 32-битный Launcher.exe, "
                  "беру первый (возможно неправильный)")

        # Закрываем handle остальных
        for pid, h, _, _ in candidates:
            if pid != chosen[0]:
                kernel32.CloseHandle(h)

        self.pid, self.handle, self.base, self.wow64 = chosen
        print(f"[PICK] PID={self.pid}, base=0x{self.base:08X}, "
              f"{'32-bit' if self.wow64 else '64-bit'}")

    def close(self):
        if self.handle:
            kernel32.CloseHandle(self.handle)
            self.handle = None

    def read_bytes(self, addr, size):
        buf = (ctypes.c_ubyte * size)()
        read = ctypes.c_size_t(0)
        ok = kernel32.ReadProcessMemory(
            self.handle, ctypes.c_void_p(addr), buf, size, ctypes.byref(read)
        )
        if not ok or read.value != size:
            return None
        return bytes(buf)

    def read_u8(self, addr):
        b = self.read_bytes(addr, 1)
        return b[0] if b else None

    def read_i8(self, addr):
        b = self.read_bytes(addr, 1)
        if not b:
            return None
        return struct.unpack("<b", b)[0]

    def read_u32(self, addr):
        b = self.read_bytes(addr, 4)
        if not b:
            return None
        return struct.unpack("<I", b)[0]

    def read_f32(self, addr):
        b = self.read_bytes(addr, 4)
        if not b:
            return None
        return struct.unpack("<f", b)[0]

    def read_ptr_field_f32(self, ptr_addr, offset):
        base = self.read_u32(ptr_addr)
        if not base:
            return None
        return self.read_f32(base + offset)

    def read_ptr_field_i8(self, ptr_addr, offset):
        base = self.read_u32(ptr_addr)
        if not base:
            return None
        return self.read_i8(base + offset)


# ============================================================================
# Основной цикл
# ============================================================================
def main():
    try:
        r = Reader(PROCESS_NAME)
    except RuntimeError as e:
        print(f"[ERROR] {e}")
        return 1

    print(f"[OK] PID={r.pid}, base=0x{r.base:08X}")
    print(f"[OK] Пишу в {LOG_FILE}")
    print(f"[OK] Нажми Ctrl+C чтобы остановить")

    dt = 1.0 / TICK_HZ
    prev = None

    with open(LOG_FILE, "w", encoding="utf-8", buffering=1) as f:
        f.write("# time, kontroller, speed_kph, revers, loc36_raw, loc36_hex\n")
        f.write(f"# base=0x{r.base:08X}\n")
        f.flush()

        t_start = time.time()
        try:
            while True:
                t = time.time() - t_start

                kontr = r.read_u8(r.base + ADDR_KONTROLLER)
                speed = r.read_ptr_field_f32(
                    r.base + ADDR_SPEED_BASE_PTR, OFF_SPEED_KPH
                )
                revers = r.read_ptr_field_i8(
                    r.base + ADDR_REVERS_BASE_PTR, OFF_REVERS
                )
                loc36 = r.read_ptr_field_f32(
                    r.base + ADDR_LOCS_BASE_PTR, OFF_LOCS_36
                )

                # сырые байты loc36 для диагностики (вдруг не float)
                loc36_base = r.read_u32(r.base + ADDR_LOCS_BASE_PTR)
                loc36_hex = None
                if loc36_base:
                    raw = r.read_bytes(loc36_base + OFF_LOCS_36, 4)
                    if raw:
                        loc36_hex = raw.hex()

                row = (
                    f"{t:7.2f}, "
                    f"k={kontr!s:>3}, "
                    f"v={_fmt(speed, '7.2f')}, "
                    f"rev={revers!s:>3}, "
                    f"loc36={_fmt(loc36, '+9.4f')}, "
                    f"hex={loc36_hex or '----'}"
                )

                if LOG_ALL_TICKS or row != prev:
                    f.write(row + "\n")
                    f.flush()
                    prev = row

                time.sleep(dt)
        except KeyboardInterrupt:
            print("\n[BYE]")

    r.close()
    return 0


def _fmt(v, spec):
    if v is None:
        return "  ----"
    return format(v, spec)


if __name__ == "__main__":
    sys.exit(main())
