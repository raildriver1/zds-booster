// ----------------------------------------------------------------------------
// Addresses.pas
//
// Centralized memory addresses and button offsets used by the engine to
// patch / read ZDSimulator (Launcher.exe) state.
//
// All addresses listed here are absolute virtual addresses inside the
// ZDSimulator process and are tied to a specific build of Launcher.exe.
// If the game is updated, these values may shift and need to be re-derived.
//
// Keep this file ASCII-only so it builds cleanly in Delphi 2007 regardless
// of source encoding settings.
// ----------------------------------------------------------------------------
unit Addresses;

interface

const
  // Base addresses
  BASE_ADDRESS  = $00400000;
  ZBUTTONS_BASE = $8DD92E0;

  // KLUB variables
  KLUB_RMP_ADDR          = $49A390;
  KLUB_VVOD_MODE_ADDR    = $49A3A0;
  KLUB_BRIGHTNESS_ADDR   = $49A3D0;
  KLUB_STRING3_ADDR      = $5162840;
  KLUB_TRAIN_LENGTH_ADDR = $5162890;
  KLUB_TRACK_ADDR        = $5162860;
  KLUB_ISPRAV_ADDR       = $49A3B0;
  KLUB_K4_ADDR           = $51628A0;
  KLUB_K71_ADDR          = $51628E0;
  KLUB_SHOW_TC_ADDR      = $5162880;
  KLUB_K122_ADDR         = $51628B0;
  KLUB_K799_ADDR         = $51628C0;
  KLUB_K809_ADDR         = $51628D0;
  KLUB_WHITE_SPEED_ADDR  = $49A3C0;
  KLUB_EK_ADDR           = $5162870;

  // System variables
  WHEELSETS_SPEED_ADDR  = $516101C;
  LS_ADDR               = $8DDD320;
  ALSN_MODE_ADDR        = $51627F0;
  ALSN_FREQ_ADDR        = $49A400;
  VK_STATUS_ADDR        = $49A590;
  RB_STATUS_ADDR        = $49A5A0;
  ZTIME_HOUR_ADDR       = $8DDD510;
  ZTIME_MIN_ADDR        = $8DDD514;
  ZTIME_SEC_ADDR        = $8DDD518;
  ABS_SPEED_KPH_ADDR    = $49A680;
  TRAVERSED_PATH_ADDR   = $49A620;
  SAUT_PODT_DIST_ADDR   = $49A430;
  SAUT_OTPRAV_DIST_ADDR = $49A440;
  SAUT_AIM_DIST_ADDR    = $5162910;
  SAUT_SPEED_LIMIT_ADDR = $5162900;
  MY_TRACK_ADDR         = $49A8D0;
  MAX_TRACKS_ADDR       = $49A310;
  ROUTE_ADDR            = $49A1A0;
  KLUB_SPEED_LIMIT_ADDR = $49A360;
  CURR_SIGNAL_DIST_ADDR = $8DDD2D0;
  KLUB_MESSAGE_ADDR     = $5162850;
  SAUT_SS_SPEED_ADDR    = $5162920;
  TRACK_DATA_BASE_ADDR  = $516279C;

  // Button offsets (relative to ZBUTTONS_BASE)
  BTN_RMP_CURR        = $19;   BTN_RMP_PREV        = $18;
  BTN_FREQ_CURR       = $25;   BTN_FREQ_PREV       = $24;
  BTN_MODE10          = $31;
  BTN_MODE20          = $3D;
  BTN_MODE30          = $49;
  BTN_BRIGHT_CURR     = $55;   BTN_BRIGHT_PREV     = $54;
  BTN_ENTER_CURR      = $61;   BTN_ENTER_PREV      = $60;
  BTN_DIGIT0_CURR     = $79;   BTN_DIGIT0_PREV     = $78;
  BTN_DIGIT1_CURR     = $85;   BTN_DIGIT1_PREV     = $84;
  BTN_DIGIT2_CURR     = $91;   BTN_DIGIT2_PREV     = $90;
  BTN_DIGIT3_CURR     = $9D;   BTN_DIGIT3_PREV     = $9C;
  BTN_DIGIT4_CURR     = $A9;   BTN_DIGIT4_PREV     = $A8;
  BTN_DIGIT5_CURR     = $B5;   BTN_DIGIT5_PREV     = $B4;
  BTN_DIGIT6_CURR     = $C1;   BTN_DIGIT6_PREV     = $C0;
  BTN_DIGIT7_CURR     = $CD;   BTN_DIGIT7_PREV     = $CC;
  BTN_DIGIT8_CURR     = $D9;   BTN_DIGIT8_PREV     = $D8;
  BTN_DIGIT9_CURR     = $E5;   BTN_DIGIT9_PREV     = $E4;
  BTN_CANCEL          = $F1;
  BTN_SAUT_PODT_CURR  = $FD;   BTN_SAUT_PODT_PREV  = $FC;
  BTN_SAUT_OTPR_CURR  = $109;  BTN_SAUT_OTPR_PREV  = $108;
  BTN_TRACK_SPD_CURR  = $115;  BTN_TRACK_SPD_PREV  = $114;
  BTN_BRAKE_CURR      = $121;  BTN_BRAKE_PREV      = $120;

implementation

end.
