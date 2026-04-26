"""
addresses.py — централизованный список адресов и смещений ZDSimulator
(Launcher.exe), используемых отладочными скриптами вроде prikol.py.

Все адреса — абсолютные виртуальные адреса (offset от base модуля Launcher.exe)
и привязаны к конкретному билду игры. При обновлении ZDSimulator значения
могут поплыть и потребуется их заново снять.
"""

# ----------------------------------------------------------------------------
# Game memory addresses (offset from Launcher.exe base)
# ----------------------------------------------------------------------------
ADDR_KONTROLLER       = 0x08DD5B05  # byte
ADDR_LOCS_BASE_PTR    = 0x0034AEA0  # [.] + B4 = LOCSECTIONS[36] (float)
ADDR_REVERS_BASE_PTR  = 0x0034B28C  # [.] + 1C = revers (byte/int)
ADDR_SPEED_BASE_PTR   = 0x0034B41C  # [.] + 3C = speed_kph (float)

# Offsets used through the indirection above
OFF_LOCS_36   = 0xB4
OFF_REVERS    = 0x1C
OFF_SPEED_KPH = 0x3C
