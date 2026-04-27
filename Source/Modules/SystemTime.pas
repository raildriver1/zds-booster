//----------------------------------------------------------------------------//
// SystemTime.pas — ZDS-Booster system-time override                          //
//                                                                            //
// The simulator stores its current AbsoluteTime as a double (seconds since   //
// local midnight) on the heap. The address of that double is held by a       //
// static pointer at Launcher.exe+$34AEF0 (= absolute $004AAEF0 since the     //
// process loads at the standard $00400000 base — same convention as the      //
// other patches in CheatMenu.pas / EngineCore.pas).                          //
//                                                                            //
// To "use Windows time" we simply overwrite that double once per frame with  //
// the value computed from GetLocalTime. The simulator's per-frame increment  //
// happens *between* our writes and ends up being a no-op because we restore  //
// the system time the very next frame.                                       //
//                                                                            //
// Defensive checks:                                                          //
//   * IsBadReadPtr  on the static pointer slot itself (4 bytes)              //
//   * Reject nil pointer (it's nil before the simulator loads a route)       //
//   * IsBadWritePtr on the resolved heap target (8 bytes)                    //
//                                                                            //
// All of these silently no-op if anything is wrong, so the override is       //
// safe to call every frame even before the simulator has loaded a save.     //
//----------------------------------------------------------------------------//
unit SystemTime;

interface

uses Windows, Variables, SysUtils, EngineUtils;

procedure ApplySystemTimeOverride;

implementation

const
  SYSTIME_LOG = 'DGLEngine_Log.txt';

  // Static slot: 4-byte pointer to the heap-allocated double.
  PTR_ABSOLUTE_TIME = $00400000 + $34AEF0;  // = $004AAEF0

  // Throttle "address invalid" warnings to one per N frames so we don't
  // spam the log if the simulator hasn't initialised yet.
  WARN_INTERVAL_FRAMES = 600;

var
  WarnCounter: Cardinal = 0;

procedure ApplySystemTimeOverride;
var
  StaticPtr: PCardinal;
  TimeDouble: PDouble;
  st: TSystemTime;
  Seconds: Double;
begin
  if not InitSystemTimeEnable then Exit;

  // Step 1: read the static pointer slot.
  StaticPtr := PCardinal(PTR_ABSOLUTE_TIME);
  if IsBadReadPtr(Pointer(StaticPtr), 4) then Exit;

  // Step 2: dereference to get the heap pointer to the double.
  TimeDouble := PDouble(StaticPtr^);
  if TimeDouble = nil then Exit;
  if IsBadWritePtr(Pointer(TimeDouble), 8) then
  begin
    Inc(WarnCounter);
    if (WarnCounter = 1) or ((WarnCounter mod WARN_INTERVAL_FRAMES) = 0) then
      AddToLogFile(SYSTIME_LOG,
        'SystemTime: heap target not writable yet, retrying...');
    Exit;
  end;
  WarnCounter := 0;

  // Step 3: compute "seconds since local midnight" with millisecond precision.
  GetLocalTime(st);
  Seconds := st.wHour * 3600.0 + st.wMinute * 60.0 + st.wSecond
             + st.wMilliseconds * 0.001;

  // Step 4: overwrite. The simulator will read this on its next tick and
  // any internal time-dependent state (lighting, schedule, day/night
  // textures) will roll forward toward Windows time naturally.
  TimeDouble^ := Seconds;
end;

end.
