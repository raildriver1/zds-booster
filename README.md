# ZDS-Booster

![nslF7moNeUQeEz9qRz0OMe3sGbrpGGdB7kCRRgbIbxyjKgsO2qVDYO3sPJ9uTugC3CQ5a8bptXXbgTXRYy5xeRLB (1)](https://github.com/user-attachments/assets/b6100d33-35af-4a7b-b786-6d65d76c9da1)

Модифицированный движок для ZDSimulator с расширенными возможностями отображения информации, управления камерой и настройки визуальных эффектов.

Полная версия восстановленного движка (без ZDS-Booster) - https://github.com/maisvendoo/DGLEngine

## 🚀 Возможности

### 🎥 Система свободной камеры (Freecam)
- **Включение/выключение**: `Alt + X` (включить/выключить)
- **Управление движением**: 
  - `W/S` - вперед/назад
  - `A/D` - влево/вправо  
  - `Пробел` - вверх
  - `Shift` - ускорение

### 📊 Расширенная информация в кабине (только ЧС7)
- **Основные данные (УСАВПП)**: время, дата, координаты, давление, ускорение
- **Система скорости и лимитов (САУТ)**: цифровые индикаторы скорости и допустимых ограничений
- **Ступени тяги (Уровень ступени)**: отображение текущих ступеней

### 🚦 Система светофоров АЛС-ЕН (только ЧС7)
- **Команда активации**: `137` → `ENTER` → выбор частоты (1-3) → `ENTER`
- **Визуальная индикация**: цветные блоки согласно показаниям светофоров
- **Двойная система отображения**: КЛУБ и БИЛ ПОМЕ
- **Автоматический анализ** последовательности сигналов
- **⚠️ ВАЖНО**: HookKLUB (АЛС-ЕН) работает данный момент только на локомотиве ЧС7 (айди локомотива 822)

### 🌅 Система смены текстур день/ночь
- **Автоматическая смена** в зависимости от времени суток и сезона
- **Поддержка зимы и лета** с разными временными интервалами
- **Кастомные текстуры** для кабины, пульта и других элементов

## 📁 Структура файлов

```
ZDSimulator/
├── DGLEngine.dll          # Основной модифицированный движок
├── zdbooster.cfg          # Главный конфиг (автосоздается)
├── data/
│   └── [тип_локомотива]/
│       └── [номер]/
│           ├── loc/                    # Кастомные модели
│           │   ├── kolpara_*.dmd      # Модели колпаков
│           │   ├── klub_ls_*.dmd      # Модели светофоров АЛС
│           │   ├── strelka_skor.dmd   # Стрелка скорости
│           │   ├── pisec1.dmd         # Писец 1
│           │   ├── pisec2.dmd         # Писец 2
│           │   └── ...
│           └── raildriver/            # Конфиги отображения
│               ├── booster.txt        # В данный момент только для ЧС7 
└── routes/
    └── [название_маршрута]/
        └── textures/
            ├── sky_sunriseDawn.bmp          # Предрассветные сумерки
            ├── sky_sunsetTwilight.bmp       # Закатные сумерки
            ├── sky_sunriseDawn_snow.bmp     # Зимние варианты
            ├── sky_sunsetTwilight_snow.bmp # Зимние варианты
        └── models/
            ├── sky.dmd      # Модель для буста дальности видимости

```

### Конфиг отображения `raildriver/booster.txt`
```ini
# Настройки отображения элементов
saut: 0     # Отображение данных САУТ
bgsd: 1     # Отображение монитора УСАВПП
stupen: 1   # Отображение уровня ступени
```

> **⚠️ Важно**: Система HookKLUB (отображение информации в кабине и АЛС-ЕН) работает только на ЧС7. Остальные функции (фрикам, новое небо и т.д) работают на всех локомотивах.

## 🛠️ Установка

1. **Резервная копия**: Сохраните оригинальный `DGLEngine.dll`
   ```bash
   copy DGLEngine.dll DGLEngine_backup.dll
   ```
2. **Замена файла**: Скопируйте модифицированный `DGLEngine.dll` в папку игры
3. **Настройка**: При первом запуске автоматически создастся `zdbooster.cfg`
4. **Кастомизация**: Добавьте кастомные модели и текстуры по желанию

## 🎮 Управление

### Основное
- `F12` - Меню настроек (если доступно)
- `Alt + X` - Включить фрикам
- `Alt + X` - Выключить фрикам
- `К137` + `Enter` - Система АЛС-ЕН (только ЧС7)

### В режиме фрикама
- `W/A/S/D` - Движение
- `Пробел` - Подъем
- `Shift` - Ускорение

### Основная камера
- `Шаг вперёд` - Шаг вперед/назад в кабине
- `Новый Zoom` - Плавный настраеваемый Zoom
- `Чувствительность` - Чувствительность внутриигровой камеры 

## 🔧 HookKLUB (только ЧС7)

### Функции HookKLUB (для call патча)
```pascal
procedure HookKLUB(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall; export;
```

```pascal
procedure HookSkorostemerCHS7(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
);
```

## 🐛 Диагностика

### Лог файлы
- `DGLEngine_Log.txt` - Основной лог движка
- Содержит информацию о:
  - Загрузке моделей и текстур
  - Активации систем
  - Ошибках инициализации
  - Типе локомотива

### Типичные проблемы

1. **HookKLUB не работает**
   ```
   Проверьте лог: "Hook activated for locomotive type 822 (CS7)"
   Если нет - используете не ЧС7
   ```

2. **Фрикам не активируется**
   ```
   Проверьте: freecam: 1 в zdbooster.cfg
   Лог: "Freecam initialized for locomotive type"
   ```

3. **Не загружаются модели**
   ```
   Проверьте структуру: data/chs7/[номер]/loc/
   Лог: "Модель [название] загружена, ID: [число]"
   ```

4. **Не меняются новые текстуры карусели неба**
   ```
   Проверьте: есть ли «sky_sunriseDawn.bmp»; «sky_sunsetTwilight.bmp»; «sky_sunriseDawn_snow.bmp»; «sky_sunsetTwilight_snow.bmp» в папке textures в маршруте
   Запущена ли функция «Новая логика неба»
   ```

## ⚠️ Важные замечания

- **Совместимость**: HookKLUB протестирован только на ЧС7 (тип 822)
- **Безопасность**: Всегда делайте резервные копии оригинальных файлов
- **Обновления**: Следите за обновлениями для поддержки новых локомотивов

## 🔍 Техническая информация

### Требования к модифкации кода
```
IDE: CodeGear Delphi 2007
```

### Методы патчинга
1. **VirtualProtect** - изменение защиты памяти
2. **Перехват вызовов** - модификация адресов функций  
3. **Инъекция кода** - добавление новых функций

### Требования
- Windows 10/11
- ZDSimulator
- OpenGL 1.x+
- Права администратора (для патчинга памяти)

## 📞 Поддержка

При возникновении проблем:
1. Проверьте `DGLEngine_Log.txt`
2. Убедитесь в правильности конфигурации
3. Для ЧС7: проверьте активацию HookKLUB в логе
4. Создайте Issue с описанием проблемы и логами
5. Напишите нам в группу https://vk.com/raildriver и скиньте DGLEngine_Log.txt

---
*ZDBooster - модификация для энтузиастов железнодорожного моделирования*  
*HookKLUB работает только на ЧС7. Остальные функции доступны на всех локомотивах.*
