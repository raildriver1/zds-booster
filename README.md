<div align="center">

# 🚂 ZDS-Booster

![hero](https://github.com/user-attachments/assets/b6100d33-35af-4a7b-b786-6d65d76c9da1)

**Модифицированный движок для ZDSimulator с фрикамом, расширенным HUD кабины,<br/>постпроцессом, кастомным небом и собственной физикой РА-3.**

[![License: GPL v2](https://img.shields.io/badge/License-GPL_v2-blue.svg?style=flat-square)](LICENSE)
[![Delphi](https://img.shields.io/badge/Delphi-2007-EE1F35?style=flat-square&logo=delphi&logoColor=white)](https://en.wikipedia.org/wiki/CodeGear_Delphi_2007)
[![Platform](https://img.shields.io/badge/Platform-Windows-0078D6?style=flat-square&logo=windows&logoColor=white)](#)
[![Engine](https://img.shields.io/badge/Engine-DGLEngine_v1.1-green?style=flat-square)](https://github.com/maisvendoo/DGLEngine)
[![Locomotive](https://img.shields.io/badge/Full_HUD-ЧС7-red?style=flat-square)](#)

[Возможности](#-возможности) ·
[Установка](#️-установка) ·
[Меню F12](#️-меню-f12-cheatmenu) ·
[Хоткеи](#-полный-список-горячих-клавиш) ·
[Конфиг](#-конфиг-zdboostercfg) ·
[Сборка](#-сборка-из-исходников)

</div>

> [!NOTE]
> ZDS-Booster подменяет штатный `DGLEngine.dll` в ZDSimulator. Это **не** автономная игра — нужна установленная копия ZDSimulator.

> [!WARNING]
> Адреса памяти жёстко привязаны к конкретному билду `Launcher.exe`. После обновления ZDSimulator может потребоваться пересборка `Source/Modules/Addresses.pas` и `addresses.py`.

---

## ⚡ TL;DR

```bat
REM 1. Бэкап оригинала
copy DGLEngine.dll DGLEngine_backup.dll

REM 2. Положи модифицированный DGLEngine.dll поверх

REM 3. Запусти ZDSimulator. F12 — меню. Alt+X — фрикам.
```

---

## 🚀 Возможности

<table>
<tr>
<td width="33%" valign="top">

### 🎨 Визуал
- 🎥 **Свободная камера** (Alt+X)
- 🌅 **Новое небо** день/ночь, зима
- ✨ **FXAA** + **Bloom** постпроцесс
- 🔆 **MSAA** + **Anisotropic**
- 🪟 Modern OpenGL context
- 💡 Тонкая настройка света и FOV

</td>
<td width="33%" valign="top">

### 📊 HUD кабины (ЧС7)
- 🧭 **УСАВПП** монитор
- 🚦 **САУТ** цифровые индикаторы
- ⚙️ **Ступень тяги**
- 🟢 **АЛС-ЕН** цветные блоки
- 📐 **Скоростемер** ЧС7
- 🎯 До 256 кастомных 3D-текстов

</td>
<td width="33%" valign="top">

### 🚆 РА-3
- ⚙️ Своя физика (дизель + ГДТ/ГДМ)
- 🎮 Виртуальный контроллер [-5..+5]
- 🚪 Анимация дверей и колёс
- 🏎️ Hook интегратора скорости
- 🟢 АЛС-ЕН индикаторы
- 🎙️ Discord Rich Presence

</td>
</tr>
</table>

Прочее: централизованный `zdbooster.cfg`, локализация **ru/uk/en**, обработчик клавиш **КЛУБ-У** с буфером ввода (`К137`/`К122`/`К123`/`К799`/`К800`), внешний логгер физики `prikol.py`.

> Полная версия восстановленного движка (без ZDS-Booster) — https://github.com/maisvendoo/DGLEngine

---

## 🛠️ Установка

<table>
<tr>
<td>1️⃣</td>
<td><b>Бэкап оригинала</b><br/>
<code>copy DGLEngine.dll DGLEngine_backup.dll</code> в папке игры.</td>
</tr>
<tr>
<td>2️⃣</td>
<td><b>Замена DLL</b><br/>
Положи модифицированный <code>DGLEngine.dll</code> поверх оригинального.</td>
</tr>
<tr>
<td>3️⃣</td>
<td><b>Первый запуск</b><br/>
ZDSimulator стартует, автоматически создаётся <code>zdbooster.cfg</code> рядом с <code>Launcher.exe</code>.</td>
</tr>
<tr>
<td>4️⃣</td>
<td><b>Кастомизация (опционально)</b><br/>
• Положи модели РА-3 из <code>ra3/</code> в подпапки <code>data/</code><br/>
• Сделай <code>data/&lt;тип&gt;/&lt;номер&gt;/raildriver/booster.txt</code> с нужными флагами<br/>
• Положи 4 файла <code>sky_*.bmp</code> в <code>routes/&lt;маршрут&gt;/textures/</code> для нового неба</td>
</tr>
<tr>
<td>5️⃣</td>
<td><b>Перезапуск ZDSimulator</b> — готово.</td>
</tr>
</table>

---

## ⚙️ Меню F12 (CheatMenu)

Внутриигровое меню — основной способ управлять всеми тумблерами и слайдерами. Реализовано как несколько перетаскиваемых окон с современным тёмным UI (`Source/CheatMenu.pas`, ~4150 строк). Все настройки применяются live, без рестарта.

<details>
<summary><b>🎨 Окно «Render»</b></summary>

| Группа | Тумблер / слайдер | Что делает |
|--------|-------------------|------------|
| Free Camera | `freecam` | Toggle фрикама (Alt+X тоже работает) |
| ↳ Базовая скорость | `basespeed` | Скорость движения фрикама |
| ↳ Быстрая скорость | `fastspeed` | Скорость с зажатым `Shift` |
| ↳ Скорость поворота | `turnspeed` | Чувствительность поворота |
| Main Camera | `main_camera` | Тюнинг основной камеры |
| ↳ Step Forward | `stepforward` | Шаг камеры в кабине |
| ↳ View Angle | `view_angle` | Множитель FOV (патч игры) |
| ↳ Camera Sensitivity | `camera_sensitivity_value` | Чувствительность мыши |
| Lighting | — | Раскрываемая секция |
| ↳ Main / Additional Light | — | Интенсивность источников |
| ↳ Cabin Brightness / Contrast | — | Яркость и контраст кабины |
| ↳ Sun Orbit Radius / Height | — | Положение солнца |
| Brightness | `brightness` | Глобальная яркость |

</details>

<details>
<summary><b>🌍 Окно «World»</b></summary>

| Тумблер | Ключ | Описание |
|---------|------|----------|
| Max Visible Distance | `max_distance` + `maxvisibledistance` | Расширенная дальность отрисовки (м) |
| Show Wires | `show_wires` | Дальние провода |
| Show Distant Models | `show_distant_models` | Дальние модели зданий/деревьев |
| Show Traffic Lights | `show_traffic_lights` | Дальние светофоры |
| New Sky | `newsky` | Новая логика неба с подменой текстур |
| FXAA | — | Постпроцесс-сглаживание |
| Bloom | — | Постпроцесс-блум |

</details>

<details>
<summary><b>🚂 Окно «Locomotive»</b></summary>

| Тумблер | Ключ | Описание |
|---------|------|----------|
| Club Fixes | `new_club_positions` | Исправление позиций индикаторов КЛУБ на ЭД4М/ЧС7/ЧС8 |
| RA3 Hover | `ra3_hover` | Подсветка интерактивных элементов в кабине РА-3 |
| Developer Menu | — | Расширенный режим разработки + гизмо для кастомных текстов |

</details>

<details>
<summary><b>🧩 Кастомные 3D-тексты в кабине РА-3</b></summary>

В кабине РА-3 можно разместить до 256 произвольных 3D-текстов (источник из переменных КЛУБ/САУТ/УСАВПП), каждый со своим:

- Position **X / Y / Z**
- Rotation **X / Y / Z**
- **Scale**
- Видимость
- Источник данных (`TKlubSource`: скорость, давление, время, координаты, и т.п.)

Редактирование — через **гизмо-контроллер**: Translate (стрелки), Rotate (кольца), Scale (кубики). Гизмо рисуется даже при закрытом меню F12, чтобы можно было «как РА-3 контроллер» крутить позицию мышью.

</details>

---

## 🎥 Свободная камера (Freecam)

Реализована в `Source/Modules/DrawFunc3D.pas` через NOP-патч игровой проверки + запись 0/1 в `FREEMODE_SWITCH_ADDR`.

| Клавиша | Действие |
|---------|----------|
| `Alt + X` | Включить / выключить фрикам |
| `W` / `S` | Вперёд / назад |
| `A` / `D` | Влево / вправо |
| `Space` | Подняться |
| `Shift` | Ускорение (использует `Fast speed` из меню) |

При выключении фрикама камера возвращается в исходную позицию. Состояние сохраняется в `freecam: 0/1` в конфиге.

---

## 📊 HUD кабины (HookKLUB)

Расширенный визуальный HUD. Подключается через экспортируемую функцию `HookKLUB`, которую игра зовёт каждый кадр для каждой панели.

> [!IMPORTANT]
> HookKLUB (АЛС-ЕН и связанный HUD) полноценно работает **только на ЧС7 (тип локомотива 822)**. На остальных локомотивах активны только общие функции — фрикам, новое небо, FXAA/Bloom, тюнинг камеры.

**Что отображается:**

- 🧭 **УСАВПП (BGSD)** — время, дата, координаты, давление в магистрали, ускорение
- 🚦 **САУТ** — цифровые индикаторы скорости и допустимых ограничений
- ⚙️ **Уровень ступени тяги** (`stupen`) — текущая позиция контроллера машиниста
- 🟢 **АЛС-ЕН** — цветные блоки светофора на КЛУБ и БИЛ ПОМЕ
- 📐 **Скоростемер ЧС7** — отдельный hook через `HookSkorostemerCHS7`

Включение элементов делается флагами в `data/<тип>/<номер>/raildriver/booster.txt`:

```ini
saut: 0     # Отображение данных САУТ
bgsd: 1     # Отображение монитора УСАВПП
stupen: 1   # Отображение уровня ступени тяги
```

---

## 🚦 АЛС-ЕН и КЛУБ-У

Собственный обработчик клавиш КЛУБ-У реализован в `Source/Modules/DrawFunc3D.pas` (state machine с буфером `InputBuffer`). Также есть отдельный модуль `Source/KlubProcessor.pas` — он использует таблицу адресов из `Source/Modules/Addresses.pas`.

Поддерживаемые команды (вводятся как `K<число>` → `Enter`):

| Команда | Что делает |
|---------|------------|
| `К137` | ✅ Активация управления АЛС-ЕН (state 52). Дальше: частота 1-3 → `Enter` |
| `К122` | Установить байт по `0x0538D95F` в `1` |
| `К123` | Сбросить байт по `0x0538D95F` в `0` |
| `К799` + число | Перейти в state 31, записать DWORD по `0x00749894` |
| `К800` | Сбросить DWORD по `0x0538D960` в `0` |
| _Любая другая_ | ❌ Буфер сбрасывается, лог пишет «✗ Неизвестная команда» |

Все события пишутся в `DGLEngine_Log.txt` с префиксами `[К-ВВОД]`, `[КНОПКА СТР]` и т.п.

---

## 🚆 РА-3: модели и физика

ZDS-Booster содержит самостоятельную реализацию РА-3 поверх движка ЭД4М (`Source/Modules/RA3.pas` + `Source/Modules/RA3Physics.pas`).

### Модели и текстуры

Хранятся в `ra3/`:

| Папка | Что внутри |
|-------|------------|
| `ra3/loco/` | Секции локомотива (Material #N.NNN) |
| `ra3/wagon/` | Модели вагона + колёсная тележка |
| `ra3/cab/` | Кабина (material0..material6) |
| `ra3/dynamic/` | Анимированные элементы (см. ниже) |

<details>
<summary><b>Список анимированных элементов (ra3/dynamic/)</b></summary>

| Файл | Что это |
|------|---------|
| `kontroller.dmd` / `controller_braking.dmd` | Рычаг контроллера машиниста |
| `arrow_PM.dmd` | Стрелка манометра ПМ |
| `arrow_TC1.dmd` / `arrow_TC2.dmd` | Стрелки манометров ТЦ |
| `button_RB.dmd` / `button_RBP.dmd` / `button_RBS.dmd` | Кнопки бдительности |
| `indicator_LS_G1_M.dmd`..`G4_M.dmd` | Индикаторы АЛС-ЕН зелёные (1-4 блока) |
| `indicator_LS_R_M.dmd` | Индикатор АЛС-ЕН красный |
| `indicator_LS_Y_M.dmd` | Индикатор АЛС-ЕН жёлтый |
| `indicator_LS_RY_M.dmd` | Индикатор АЛС-ЕН красно-жёлтый |
| `indicator_LS_W_M.dmd` | Индикатор АЛС-ЕН белый |

В составе предусмотрены три секции (локомотив + вагон + локомотив) с интерактивным управлением, поворотом, открытием дверей и анимацией колёсных пар.

</details>

### Физическая модель

`RA3Physics.pas` подменяет штатную физику ЭД4М:

- 🛢️ Дизельный двигатель (`disel.cpp` из `ra3-equipment` — портирован в Delphi)
- 🌀 Гидропередача (ГДТ/ГДМ/ГТ — `hydro-transmission.cpp`) с полиномиальной аппроксимацией кривых
- ↩️ Реверсор
- ⏱️ 10 Гц тик (запись в `LOCSECTIONS[36]` каждые 100 мс)

**Управление:**

| Действие | Как |
|----------|-----|
| Главный тумблер физики | `Alt + Z` → `ToggleRA3Booster` (физика + запись в `loc36` + дефолты `TractionScale=1.0`, `SpeedLimit=100`) |
| Виртуальный контроллер | `SetVirtualController(v)`, диапазон `[-5..+5]`: `0` нейтраль, `+1..+5` тяга, `-1..-5` тормоз |
| Hook интегратора скорости | `InstallSpeedHook` / `RemoveSpeedHook` — патчит `v += a*dt` в `Launcher.exe + 0x345F00`, домножая ускорение на `SpeedAccelScale` |

Диагностика — через `GetDieselRPM`, `GetM_out`, `GetRailForce`, `GetFillGT`, `GetFillGM`, `GetFillGB`, `GetLocSection36`.

---

## 🌅 Небо: новые текстуры день/ночь

Включается тумблером **New Sky** в окне «World». Подменяет небосвод собственными текстурами в зависимости от времени суток и сезона.

**Куда положить файлы:**

```
ZDSimulator/routes/<название_маршрута>/
├── textures/
│   ├── sky_sunriseDawn.bmp           # Предрассветные сумерки (лето)
│   ├── sky_sunsetTwilight.bmp        # Закатные сумерки (лето)
│   ├── sky_sunriseDawn_snow.bmp      # Зимний рассвет
│   └── sky_sunsetTwilight_snow.bmp   # Зимний закат
└── models/
    └── sky.dmd                       # Модель буста дальности
```

> [!TIP]
> Если хотя бы один из четырёх `sky_*.bmp` отсутствует, новая логика неба не активируется. Смотри `DGLEngine_Log.txt` — там пишется отсутствие файлов.

---

## 🎙️ Discord Rich Presence

`Source/DiscordRPC.pas` подключается к Discord по Named Pipe `\\.\pipe\discord-ipc-0..9`. Отображается активность с timestamp начала сессии.

- **Application ID:** `1408466421846905004`
- **Иконок** в текущей версии нет — текстовая активность
- **Логи отладки RPC** пишутся в `discord_debug.log` рядом с `Launcher.exe`

> [!NOTE]
> Discord должен быть запущен **до** ZDSimulator, иначе подключение к pipe не пройдёт.

---

## 📁 Файловая структура

<details>
<summary><b>В каталоге ZDSimulator после установки</b></summary>

```
ZDSimulator/
├── DGLEngine.dll                 # Основной модифицированный движок (этот мод)
├── zdbooster.cfg                 # Главный конфиг (автосоздаётся)
├── DGLEngine_Log.txt             # Лог движка
├── discord_debug.log             # Лог Discord RPC (если включён)
├── data/
│   └── <тип_локомотива>/
│       └── <номер>/
│           ├── loc/                   # Кастомные модели
│           │   ├── kolpara_*.dmd      # Колпачки
│           │   ├── klub_ls_*.dmd      # Светофоры АЛС-ЕН
│           │   ├── strelka_skor.dmd   # Стрелка скорости
│           │   ├── pisec1.dmd
│           │   ├── pisec2.dmd
│           │   └── ...
│           └── raildriver/            # Конфиги отображения HUD
│               └── booster.txt        # Активно для ЧС7
└── routes/
    └── <название_маршрута>/
        ├── textures/sky_*.bmp
        └── models/sky.dmd
```

</details>

<details>
<summary><b>В этом репозитории</b></summary>

```
ra3/                              # Ассеты РА-3 (модели + текстуры)
├── cab/                          # Кабина
├── dynamic/                      # Стрелки, индикаторы АЛС-ЕН, контроллер
├── loco/                         # Корпус секций локомотива
└── wagon/                        # Вагон + колёсные тележки

Source/                           # Исходники Delphi 2007
├── DGLEngine.dpr                 # Главный проект (library)
├── DGLEngine.dproj
├── CheatMenu.pas                 # Внутриигровое меню F12
├── Menu.pas                      # Старое (legacy) меню — Insert
├── KeyboardPanel.pas             # Виртуальная клавиатура (тачскрин)
├── KlubProcessor.pas             # Обёртка над клавишами КЛУБ-У
├── DiscordRPC.pas                # Discord Rich Presence
└── Modules/
    ├── Addresses.pas             # Все адреса памяти ZDSimulator
    ├── DrawFunc2D.pas            # 2D-рисование (HUD)
    ├── DrawFunc3D.pas            # 3D + freecam + АЛС-ЕН + sky (~12k строк)
    ├── EngineCore.pas            # OpenGL/окно
    ├── EngineUtils.pas
    ├── Bloom.pas / FXAA.pas      # Постпроцесс
    ├── KlubData.pas              # Данные КЛУБ для отрисовки
    ├── RA3.pas / RA3Physics.pas  # РА-3 модели и физика
    ├── Sound.pas / Net.pas
    ├── Textures.pas
    ├── Variables.pas             # Глобальные переменные движка
    └── Headers/                  # Pascal-обёртки над OpenGL/DirectX/Vfw

prikol.py                         # Внешний Python-логгер физики РА-3
addresses.py                      # Адреса памяти для prikol.py
DGLEngine_header.pas              # Заголовок для подключения как Delphi-плагина
```

</details>

---

## 🎮 Полный список горячих клавиш

| Клавиша | Где работает | Что делает |
|---------|--------------|------------|
| `F12` | Везде | Открыть/закрыть основное меню (CheatMenu) |
| `Alt + X` | Везде | Toggle фрикама |
| `Alt + Z` | Везде | Toggle РА-3 Booster (физика + запись + дефолты) |
| `W` `A` `S` `D` | В режиме фрикама | Движение |
| `Space` | В режиме фрикама | Подъём |
| `Shift` | В режиме фрикама | Ускорение |
| `Insert` | Старое меню (`Menu.pas`) | Закрыть legacy-меню |
| `K137` + `Enter` | КЛУБ-У | Управление АЛС-ЕН (далее частота 1-3 + `Enter`) |
| `K122` / `K123` | КЛУБ-У | Toggle байта по `0x0538D95F` |
| `K799` + число + `Enter` | КЛУБ-У | Записать DWORD по `0x00749894` |
| `K800` | КЛУБ-У | Сбросить DWORD по `0x0538D960` |
| ЛКМ | По меню F12 / гизмо | Перетаскивание окон, ползунков, гизмо |

---

## 📝 Конфиг `zdbooster.cfg`

Файл лежит рядом с `Launcher.exe`, формат `key: value`, по одной паре на строку. Парсится в `CheatMenu.pas → LoadConfig`. При выходе из меню значения автоматически сохраняются (`SaveConfig`).

<details>
<summary><b>Полный список ключей</b></summary>

### Состояния модулей (0/1)

| Ключ | По умолчанию | Описание |
|------|--------------|----------|
| `language` | `0` | 0 — русский, 1 — украинский, 2 — английский |
| `freecam` | `0` | Состояние фрикама |
| `main_camera` | `0` | Тюнинг основной камеры |
| `max_distance` | `0` | Расширенная дальность |
| `newsky` | `0` | Новая логика неба |
| `new_club_positions` | `0` | Патч позиций КЛУБ |
| `ra3_hover` | `0` | Подсветка интерактива РА-3 |
| `new_view_angle` | `0` | Патч FOV |
| `camera_sensitivity` | `0` | Своя чувствительность |
| `show_wires` | `0` | Дальние провода |
| `show_distant_models` | `0` | Дальние модели |
| `show_traffic_lights` | `0` | Дальние светофоры |

### Слайдеры

| Ключ | По умолчанию | Описание |
|------|--------------|----------|
| `basespeed` | `1.0` | Базовая скорость фрикама |
| `fastspeed` | `2.0` | Скорость фрикама с `Shift` |
| `turnspeed` | `1.5` | Скорость поворота фрикама |
| `stepforward` | `0.5` | Шаг камеры в кабине |
| `maxvisibledistance` | `1200` | Дальность отрисовки (м) |
| `view_angle` | `3.0` | Множитель FOV |
| `camera_sensitivity_value` | `5.0` | Чувствительность камеры |
| `brightness` | `0.0` | Глобальная яркость |
| `gizmo_mode` | `0` | 0 — Translate, 1 — Rotate, 2 — Scale |

### Кастомные 3D-тексты в кабине РА-3

| Ключ | Описание |
|------|----------|
| `custom_text_count` | Количество кастомных текстов (max 256) |
| `ct<N>_source` | Источник данных для текста N (`TKlubSource`) |
| `ct<N>_x` / `_y` / `_z` | Позиция в локальной системе кабины |
| `ct<N>_rx` / `_ry` / `_rz` | Поворот (default `-90/0/0`) |
| `ct<N>_scale` | Масштаб |
| `ct<N>_visible` | 0/1 — видимость |

</details>

---

## 📝 Конфиг `booster.txt` (на локомотив)

Лежит в `data/<тип>/<номер>/raildriver/booster.txt`. Управляет элементами HUD кабины (актуально для ЧС7):

```ini
saut: 0     # Отображение данных САУТ
bgsd: 1     # Отображение монитора УСАВПП
stupen: 1   # Отображение уровня ступени тяги
```

---

## 🐍 prikol.py — внешний логгер физики

Скрипт на чистом Python 3.x (только `ctypes` + Windows API), читает физические переменные из `Launcher.exe` и пишет в `prikol.log` рядом со скриптом.

```cmd
python prikol.py            REM обычное логирование (10 Гц, только при изменениях)
python prikol.py -d         REM диагностический дамп памяти + sweep LOCSECTIONS
```

**Что читает:**

- `kontroller` (byte) — позиция контроллера
- `LOCSECTIONS[36]` (float) — секция, которую перезаписывает физика РА-3
- `revers` (byte/int) — реверсор
- `speed_kph` (float) — текущая скорость

Все адреса вынесены в `addresses.py` — там же лежат оффсеты `OFF_LOCS_36`, `OFF_REVERS`, `OFF_SPEED_KPH`. Если игра обновится и адреса поплывут — правится только `addresses.py`.

> [!IMPORTANT]
> Запускать от администратора, если игра запущена от админа (иначе `OpenProcess` вернёт `ACCESS_DENIED`).

---

## 🐛 Диагностика

### Лог-файлы

| Файл | Откуда | Что внутри |
|------|--------|-----------|
| `DGLEngine_Log.txt` | Движок | Загрузка моделей/текстур, активация подсистем, ошибки, тип локомотива, события КЛУБ-У |
| `discord_debug.log` | DiscordRPC | Попытки подключения к pipe, ответы Discord |
| `prikol.log` | `prikol.py` | Изменения kontroller / LOCSECTIONS[36] / revers / speed |

<details>
<summary><b>Типичные проблемы и решения</b></summary>

**HookKLUB не работает**
- Проверь лог: должно быть `Hook activated for locomotive type 822 (CS7)`
- Если нет — текущий локомотив не ЧС7, для других пока не поддерживается полный HUD

**Фрикам не активируется**
- Проверь `freecam: 1` в `zdbooster.cfg`
- В логе должно быть `Freecam initialized for locomotive type ...`
- При неудачном patch видно `ОШИБКА: Не удалось применить NOP patch`

**Не загружаются модели**
- Проверь структуру: `data/<тип>/<номер>/loc/`
- В логе строка `Модель <название> загружена, ID: <число>`

**Не меняются текстуры неба**
- Все 4 файла должны быть на месте: `sky_sunriseDawn.bmp`, `sky_sunsetTwilight.bmp`, `sky_sunriseDawn_snow.bmp`, `sky_sunsetTwilight_snow.bmp`
- В UI должна быть включена «New Sky»

**РА-3 физика не запускается**
- Нажми `Alt + Z` (`ToggleRA3Booster`)
- Проверь, что играешь именно за РА-3
- Лог покажет включение `SpeedHook` и активацию записи в `LOCSECTIONS[36]`

**Discord RPC не подключается**
- Discord должен быть запущен **до** ZDSimulator
- Смотри `discord_debug.log` — там пишутся попытки подключения к pipe `discord-ipc-0..9`

</details>

---

## 🔧 HookKLUB — экспортируемые функции

DLL экспортирует две процедуры, которые игра зовёт через call-патч:

```pascal
procedure HookKLUB(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall; export;

procedure HookSkorostemerCHS7(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
);
```

Параметры — позиция и поворот текущей панели (КЛУБ или скоростемера) в системе кабины. Внутри функции по адресам из `Source/Modules/Addresses.pas` читаются текущие значения переменных и рисуются OpenGL-средствами поверх кабины.

Полный список экспортируемых имён DLL — в `Source/DGLEngine.dpr → exports`.

---

## 🔍 Сборка из исходников

### Требования

- 🛠️ **CodeGear Delphi 2007** (более новые версии не тестировались)
- 🪟 Windows 10/11
- (Для запуска полученной DLL) ZDSimulator, OpenGL 1.x+, права администратора (для патчинга памяти)

### Шаги

1. Установи CodeGear Delphi 2007.
2. Открой `Source/DGLEngine.dproj` в IDE.
3. **Project → Options → Directories/Conditionals** — убедись, что в `Search path` есть `Modules\` и `Modules\Headers\`.
4. Выбери конфигурацию **Release**.
5. **Project → Build**. Готовая `DGLEngine.dll` появится в выходной папке проекта.
6. Положи её в каталог ZDSimulator поверх оригинальной (с предварительным бэкапом).

<details>
<summary><b>Методы патчинга (для разработчика)</b></summary>

Используется три приёма:

1. **`VirtualProtect`** — изменение защиты страниц памяти (для записи)
2. **Перехват вызовов** — модификация адреса в `call <addr>` инструкциях, чтобы прыгать в наш код вместо игрового
3. **Инъекция кода** — добавление новых функций через export DLL и подмена адресов в импорт-таблице

Все используемые адреса собраны в `Source/Modules/Addresses.pas` — при обновлении игры именно этот файл нужно править в первую очередь.

</details>

---

## ⚠️ Совместимость и ограничения

- 🎯 **Версия игры**: мод привязан к конкретному билду `Launcher.exe` (адреса памяти жёсткие). После обновления ZDSimulator адреса могут поплыть и нужно пересобрать `Source/Modules/Addresses.pas` и `addresses.py`.
- 🚂 **HookKLUB (HUD кабины + АЛС-ЕН)**: полноценно работает только на **ЧС7 (тип 822)**. На остальных локомотивах активны только общие функции — фрикам, новое небо, FXAA/Bloom, тюнинг камеры.
- 💾 **Безопасность**: всегда делай резервные копии оригинальных файлов перед заменой DLL.
- 🛡️ **Антивирус**: модификация памяти чужого процесса (`VirtualProtect` + `WriteProcessMemory`) часто триггерит эвристику — добавляй ZDSimulator в исключения, если AV ругается.

---

## 📞 Поддержка

При проблемах:

1. 📄 Проверь `DGLEngine_Log.txt`.
2. 📁 Убедись, что конфиг и структура папок корректны.
3. 🚂 Для ЧС7 — проверь активацию HookKLUB в логе.
4. 🐛 Создай [Issue](https://github.com/raildriver1/zds-booster/issues) с описанием и логами.
5. 💬 Или напиши в группу VK: https://vk.com/raildriver — со скриншотом `DGLEngine_Log.txt`.

---

<div align="center">

**ZDS-Booster** — модификация для энтузиастов железнодорожного моделирования.

_HookKLUB и АЛС-ЕН HUD работают только на ЧС7 (тип 822). Остальные функции доступны на всех локомотивах._

[⬆ Наверх](#-zds-booster)

</div>
