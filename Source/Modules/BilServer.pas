unit BilServer;

interface

uses
  Windows, Winsock, SysUtils, StrUtils;

var
  BilServerRunning: Boolean = False;
  BilServerPort: Word = 5000;
  BilServerIPCount: Integer = 0;
  BilServerIPs: array[0..7] of string;
  BilBlock160: Boolean = False;

procedure BilServer_Start;
procedure BilServer_Stop;
function  BilServer_GetAddressCount: Integer;
function  BilServer_GetAddress(Index: Integer): string;

implementation

var
  ServerThread: THandle = 0;
  ServerSocket: Integer = -1;
  StopFlag: Boolean = False;

procedure CollectLocalIPs;
var
  HostEntry: PHostEnt;
  Addr: In_Addr;
  Buffer: array[0..63] of AnsiChar;
  AddrList: ^PInAddr;
  IPStr: string;
  i: Integer;
begin
  BilServerIPCount := 0;
  for i := 0 to High(BilServerIPs) do
    BilServerIPs[i] := '';

  if GetHostName(Buffer, SizeOf(Buffer)) <> 0 then Exit;
  HostEntry := gethostbyname(Buffer);
  if HostEntry = nil then Exit;

  AddrList := Pointer(HostEntry^.h_addr_list);
  while (AddrList^ <> nil) and (BilServerIPCount <= High(BilServerIPs)) do
  begin
    Addr := AddrList^^;
    IPStr := string(inet_ntoa(Addr));
    if (IPStr <> '127.0.0.1') and (IPStr <> '') then
    begin
      BilServerIPs[BilServerIPCount] := IPStr;
      Inc(BilServerIPCount);
    end;
    Inc(AddrList);
  end;

  if BilServerIPCount = 0 then
  begin
    BilServerIPs[0] := '127.0.0.1';
    BilServerIPCount := 1;
  end;
end;

function Cp1251ToUtf8(const Src: AnsiString): AnsiString;
var
  WLen, ULen: Integer;
  WBuf: array[0..255] of WideChar;
  UBuf: array[0..767] of AnsiChar;
begin
  Result := '';
  if Length(Src) = 0 then Exit;
  WLen := MultiByteToWideChar(1251, 0, PAnsiChar(Src),
    Length(Src), @WBuf[0], 256);
  if WLen = 0 then Exit;
  ULen := WideCharToMultiByte(65001, 0, @WBuf[0], WLen,
    @UBuf[0], 768, nil, nil);
  if ULen = 0 then Exit;
  SetLength(Result, ULen);
  Move(UBuf[0], Result[1], ULen);
end;

function ReadStationName: AnsiString;
const
  BASE = $00400000;
var
  BaseAddr, NameAddr, PiketAddr: Cardinal;
  StCount, NameLen: Byte;
  CurrentPiket, StPiket, Dist, MinDist: Integer;
  i: Integer;
  Buf: array[0..63] of AnsiChar;
  Best: AnsiString;
begin
  Result := '';
  try
    CurrentPiket := PWord(BASE + $8C08054)^;
    BaseAddr := PCardinal(BASE + $403AEC)^;
    if BaseAddr = 0 then Exit;
    StCount := PByte(BaseAddr - $04)^;
    if StCount = 0 then Exit;
    MinDist := MaxInt;
    Best := '';
    for i := 0 to StCount - 1 do
    begin
      try
        NameAddr := BaseAddr + $70 + Cardinal(i) * $48;
        NameLen := PByte(NameAddr)^;
        if NameLen > 63 then NameLen := 63;
        if NameLen = 0 then Continue;
        FillChar(Buf, SizeOf(Buf), 0);
        Move(Pointer(NameAddr + 1)^, Buf, NameLen);
        PiketAddr := BaseAddr + $48 + Cardinal(i) * $48;
        StPiket := PInteger(PiketAddr)^;
        Dist := Abs(CurrentPiket - StPiket);
        if (Dist <= 50) and (Dist < MinDist) then
        begin
          MinDist := Dist;
          SetLength(Best, NameLen);
          Move(Buf, Best[1], NameLen);
        end;
      except
        Continue;
      end;
    end;
    Result := Cp1251ToUtf8(Best);
  except
    Result := '';
  end;
end;

function BuildJsonData: AnsiString;
const
  BASE = $00400000;
var
  Speed, LimitSpeed, Distance, TargetSpeed: Integer;
  ALS, TrackNum, Mode, PP, BlockUch, Vigil, Kasseta, RevByte: Byte;
  TM, UR: Single;
  Accel: Double;
  Hour, Minute, Second: Integer;
  CoordVal: Int64;
  CoordStr: AnsiString;
  URAddr: Cardinal;
  RevStr, StationStr: AnsiString;
  AlsEn: Integer;
begin
  try
    Speed := Round(Abs(PSingle(BASE + $04F8C28C)^));
    LimitSpeed := PWord(BASE + $34987C)^;
    Distance := Abs(PInteger(BASE + $8C07EB8)^);
    ALS := PByte(BASE + $8C07ECC)^;
    TM := PSingle(BASE + $8D10738)^;
    TargetSpeed := PWord(BASE + $349880)^;
    TrackNum := PByte(BASE + $4F8D958)^;
    Mode := PByte(BASE + $349888)^;
    PP := PByte(BASE + $349890)^;
    BlockUch := PByte(BASE + $34988C)^;
    Vigil := PByte(BASE + $349894)^;
    Kasseta := PByte(BASE + $34989C)^;
    RevByte := PByte(BASE + $3498A0)^;
    Accel := PDouble(BASE + $3498B8)^;

    // UR pressure with pointer chasing
    UR := 0.0;
    try
      URAddr := PCardinal(BASE + $8D10D78)^;
      if URAddr <> 0 then
        UR := PSingle(URAddr + $20)^;
    except
      UR := 0.0;
    end;

    // Time
    Hour := PInteger(BASE + $8C08034)^;
    Minute := PInteger(BASE + $8C08038)^;
    Second := PInteger(BASE + $8C0803C)^;

    // Coordinate as XXXX.XXX
    CoordVal := Round(Abs(PDouble(BASE + $403F50)^));
    if CoordVal > 0 then
      CoordStr := AnsiString(Format('%.7d', [CoordVal]))
    else
      CoordStr := '';
    if Length(CoordStr) >= 4 then
      CoordStr := Copy(CoordStr, 1, Length(CoordStr) - 3) + '.' +
                  Copy(CoordStr, Length(CoordStr) - 2, 3);

    // Reverse
    if Speed > 0 then
    begin
      if RevByte = 0 then RevStr := 'FWD' else RevStr := 'REV';
    end
    else
      RevStr := '';

    // Station
    StationStr := ReadStationName;

    // ALS-EN value: always pass block_uch
    AlsEn := BlockUch;

    Result := AnsiString(
      '{' +
      '"speed":"' + Format('%.3d', [Speed]) + '",' +
      '"speed_limit":"' + Format('%.3d', [LimitSpeed]) + '",' +
      '"distance":"' + Format('%.4d', [Distance]) + '",' +
      '"als":' + IntToStr(ALS) + ',' +
      '"tm_pressure":"' + Format('%.1f', [TM]) + '",' +
      '"ur_pressure":"' + Format('%.1f', [UR]) + '",' +
      '"time":"' + Format('%.2d:%.2d:%.2d', [Hour, Minute, Second]) + '",' +
      '"coordinate":"' + string(CoordStr) + '",' +
      '"acceleration":"' + Format('%.2f', [Accel]) + '",' +
      '"station":"' + string(StationStr) + '",' +
      '"np":' + IntToStr(TrackNum) + ',' +
      '"pp":' + IntToStr(PP) + ',' +
      '"mode":' + IntToStr(Mode) + ',' +
      '"target_speed":' + IntToStr(TargetSpeed) + ',' +
      '"block_uch":' + IntToStr(BlockUch) + ',' +
      '"alsen":' + IntToStr(AlsEn) + ',' +
      '"vigillance":' + IntToStr(Vigil) + ',' +
      '"kasseta":' + IntToStr(Kasseta) + ',' +
      '"reverse":"' + string(RevStr) + '",' +
      '"color_class":"als-white",' +
      '"block160":' + IfThen(BilBlock160, 'true', 'false') +
      '}');
  except
    Result := '{"speed":"000","speed_limit":"000","als":1,' +
      '"tm_pressure":"0.0","ur_pressure":"0.0",' +
      '"time":"00:00:00","coordinate":"",' +
      '"acceleration":"0.0","station":"",' +
      '"np":0,"pp":0,"mode":0,"target_speed":0,' +
      '"distance":"0000","block_uch":0,"alsen":0,' +
      '"vigillance":1,"kasseta":0,' +
      '"reverse":"","color_class":"als-white"}';
  end;
end;

function GetBilHtml: AnsiString;
begin
  Result := AnsiString(
    '<!DOCTYPE html><html lang="ru"><head><meta charset="UTF-8">' +
    '<meta name="viewport" content="width=device-width,initial-scale=1.0">' +
    '<title>BIL-V</title><style>' +
    '@font-face{font-family:"KLUBU";src:url("https://raw.githubusercontent.com/roflandev/zdklub-mobile/refs/heads/main/KLUBU.ttf") format("truetype")}' +
    '@font-face{font-family:"SSegment";src:url("https://raw.githubusercontent.com/roflandev/zdklub-mobile/refs/heads/main/SSegment.ttf") format("truetype")}' +
    'body{background:black;margin:0;padding:0}' +
    'img{width:100%;display:block}' +
    '.als-rectangle{position:absolute;width:9.7vw;height:2.5vh;left:19%;transform:translate(-50%,-50%);box-sizing:border-box}' +
    '@keyframes blink{0%{opacity:1}50%{opacity:0}100%{opacity:1}}' +
    '.blink{animation:blink 1s infinite}' +
    '.als-white{background-color:white;top:46.5%}' +
    '.als-red{background-color:red;top:42.5%}' +
    '.als-yellowred{background:linear-gradient(to bottom,yellow 50%,red 50%);top:38.5%}' +
    '.als-yellow{background-color:yellow;top:34.5%}' +
    '.als-green{background-color:green;position:absolute;left:19%;width:9.7vw;height:2.5vh;transform:translate(-50%,-50%);display:none;border:2px solid black;top:30.5%}' +
    '.als-green2{background-color:green;position:absolute;top:26.5%;left:19%;width:9.7vw;height:2.5vh;transform:translate(-50%,-50%);display:none;border:2px solid black}' +
    '.als-green3{background-color:green;position:absolute;top:23.5%;left:19%;width:9.7vw;height:2.5vh;transform:translate(-50%,-50%);display:none;border:2px solid black}' +
    '.als-green4{background-color:green;position:absolute;top:20%;left:19%;width:9.7vw;height:2.5vh;transform:translate(-50%,-50%);display:none;border:2px solid black}' +
    '.container{width:100%;position:relative;text-align:center;color:yellow;font-family:Arial,sans-serif}' +
    '.coordinate,.time,.distance,.speed,.speedLimit,.svetofor,.signal,.tm,.ur,.ek,.np,.uscr,.mode,.station,.alsen{' +
    'font-family:"KLUBU",Arial,sans-serif;position:absolute;transform:translate(-50%,-50%);font-size:2.9vw}' +
    '.speed,.speedLimit{font-style:italic;font-family:"SSegment",Arial,sans-serif;font-size:8vw}' +
    '.coordinate{top:10.7%;left:33.4%;letter-spacing:0.1em}' +
    '.station{top:10.7%;left:53%}' +
    '.time{top:10.7%;left:72.9%;letter-spacing:0.14em}' +
    '.distance{top:56.4%;left:20.5%;letter-spacing:0.11em}' +
    '.svetofor{top:56.4%;left:37%}.signal{top:56.4%;left:82%}' +
    '.tm{left:19%;top:63%}.ur{left:30%;top:63%}.ek{left:49%;top:63%}' +
    '.alsen{left:40%;top:63%}.np{left:58%;top:63%}.uscr{left:68%;top:63%}' +
    '.mode{left:86.5%;top:5.6%;font-family:Arial,sans-serif;font-size:4vw}' +
    '.forward,.backward{position:absolute;left:54.8%;font-size:3vw;' +
    'transform:translate(-50%,-50%);color:yellow;display:none}' +
    '.forward{top:42.7%}.backward{top:45%}' +
    '.speed{top:28.6%;left:55.5%;letter-spacing:0.07em}' +
    '.speedLimit{top:35%;left:55.5%;color:red;letter-spacing:0.07em}' +
    '.speedL_m,.speedT_m,.speed_m{width:5%;max-width:300px;height:auto;position:absolute;transform:translate(-50%,-50%)}' +
    '.speed_0{left:48%;top:42.3%}.speed_5{left:46.3%;top:41.9%}.speed_10{left:44.6%;top:41.3%}.speed_15{left:43%;top:40.8%}.speed_20{left:41.2%;top:40.2%}.speed_25{left:39.6%;top:39.5%}.speed_30{left:38%;top:38.5%}.speed_35{left:36.6%;top:37.4%}' +
    '.speed_40{left:35.3%;top:36.3%}.speed_45{left:34.3%;top:35%}.speed_50{left:33.9%;top:33.5%}.speed_55{left:33.7%;top:32%}.speed_60{left:34%;top:30.4%}.speed_65{left:34.4%;top:29%}.speed_70{left:35.3%;top:27.7%}.speed_75{left:36.5%;top:26.6%}' +
    '.speed_80{left:37.9%;top:25.5%}.speed_85{left:39.5%;top:24.5%}.speed_90{left:41.2%;top:23.6%}.speed_95{left:42.9%;top:22.9%}.speed_100{left:44.6%;top:22.5%}.speed_105{left:46.3%;top:22%}.speed_110{left:48%;top:21.6%}' +
    '.speed_115{left:49.7%;top:21.3%}.speed_120{left:51.5%;top:21.1%}.speed_125{left:53.2%;top:21%}.speed_130{left:55%;top:21%}.speed_135{left:56.7%;top:21.1%}.speed_140{left:58.4%;top:21.3%}.speed_145{left:60.1%;top:21.5%}' +
    '.speed_150{left:61.7%;top:21.8%}.speed_155{left:63.5%;top:22.2%}.speed_160{left:65.2%;top:22.5%}.speed_165{left:67%;top:23%}.speed_170{left:68.7%;top:23.7%}.speed_175{left:70.1%;top:24.5%}.speed_180{left:71.8%;top:25.5%}' +
    '.speed_185{left:73%;top:26.6%}.speed_190{left:74.3%;top:27.6%}.speed_195{left:75.3%;top:29%}.speed_200{left:76%;top:30.4%}.speed_205{left:76%;top:31.9%}.speed_210{left:75.9%;top:33.5%}.speed_215{left:75.4%;top:34.9%}' +
    '.speed_220{left:74.4%;top:36.1%}.speed_225{left:73.2%;top:37.5%}.speed_230{left:71.8%;top:38.5%}.speed_235{left:70.3%;top:39.5%}.speed_240{left:68.6%;top:40.3%}.speed_245{left:66.8%;top:41%}.speed_250{left:65%;top:41.5%}' +
    '.speed_255{left:63.6%;top:41.9%}.speed_260{left:62%;top:42.3%}' +
    '.speedL_0{left:47%;top:44%}.speedL_5{left:45%;top:43.5%}.speedL_10{left:43%;top:43%}.speedL_15{left:41%;top:42.3%}.speedL_20{left:39%;top:41.6%}.speedL_25{left:37.1%;top:40.6%}.speedL_30{left:35.4%;top:39.5%}.speedL_35{left:33.9%;top:38.3%}' +
    '.speedL_40{left:32.6%;top:36.9%}.speedL_45{left:31.7%;top:35.3%}.speedL_50{left:31.1%;top:33.7%}.speedL_55{left:30.8%;top:32%}.speedL_60{left:31%;top:30.3%}.speedL_65{left:31.7%;top:28.5%}.speedL_70{left:32.7%;top:27.2%}' +
    '.speedL_75{left:34%;top:25.7%}.speedL_80{left:35.5%;top:24.5%}.speedL_85{left:37.2%;top:23.3%}.speedL_90{left:39%;top:22.4%}.speedL_95{left:41%;top:21.6%}.speedL_100{left:43%;top:20.9%}.speedL_105{left:45%;top:20.3%}' +
    '.speedL_110{left:46.9%;top:20%}.speedL_115{left:48.9%;top:19.6%}.speedL_120{left:50.9%;top:19.4%}.speedL_125{left:52.8%;top:19.3%}.speedL_130{left:54.8%;top:19.2%}.speedL_135{left:56.8%;top:19.2%}.speedL_140{left:58.8%;top:19.4%}' +
    '.speedL_145{left:60.9%;top:19.6%}.speedL_150{left:62.7%;top:19.9%}.speedL_155{left:64.7%;top:20.2%}.speedL_160{left:66.8%;top:20.9%}.speedL_165{left:68.7%;top:21.5%}.speedL_170{left:70.7%;top:22.4%}.speedL_175{left:72.5%;top:23.3%}' +
    '.speedL_180{left:74.2%;top:24.5%}.speedL_185{left:75.8%;top:25.7%}.speedL_190{left:77.1%;top:27.1%}.speedL_195{left:78%;top:28.6%}.speedL_200{left:78.6%;top:30.3%}.speedL_205{left:78.9%;top:32%}.speedL_210{left:78.6%;top:33.7%}' +
    '.speedL_215{left:78%;top:35.3%}.speedL_220{left:77.1%;top:36.8%}.speedL_225{left:75.8%;top:38.2%}.speedL_230{left:74.2%;top:39.5%}.speedL_235{left:72.5%;top:40.6%}.speedL_240{left:70.7%;top:41.5%}.speedL_245{left:68.8%;top:42.4%}' +
    '.speedL_250{left:66.8%;top:43%}.speedL_255{left:64.8%;top:43.6%}.speedL_260{left:62.8%;top:44%}' +
    '.speedT_0{left:47%;top:44%}.speedT_5{left:45%;top:43.5%}.speedT_10{left:43%;top:43%}.speedT_15{left:41%;top:42.3%}.speedT_20{left:39%;top:41.6%}.speedT_25{left:37.1%;top:40.6%}.speedT_30{left:35.4%;top:39.5%}.speedT_35{left:33.9%;top:38.3%}' +
    '.speedT_40{left:32.6%;top:36.9%}.speedT_45{left:31.7%;top:35.3%}.speedT_50{left:31.1%;top:33.7%}.speedT_55{left:30.8%;top:32%}.speedT_60{left:31%;top:30.3%}.speedT_65{left:31.7%;top:28.5%}.speedT_70{left:32.7%;top:27.2%}' +
    '.speedT_75{left:34%;top:25.7%}.speedT_80{left:35.5%;top:24.5%}.speedT_85{left:37.2%;top:23.3%}.speedT_90{left:39%;top:22.4%}.speedT_95{left:41%;top:21.6%}.speedT_100{left:43%;top:20.9%}.speedT_105{left:45%;top:20.3%}' +
    '.speedT_110{left:46.9%;top:20%}.speedT_115{left:48.9%;top:19.6%}.speedT_120{left:50.9%;top:19.4%}.speedT_125{left:52.8%;top:19.3%}.speedT_130{left:54.8%;top:19.2%}.speedT_135{left:56.8%;top:19.2%}.speedT_140{left:58.8%;top:19.4%}' +
    '.speedT_145{left:60.9%;top:19.6%}.speedT_150{left:62.7%;top:19.9%}.speedT_155{left:64.7%;top:20.2%}.speedT_160{left:66.8%;top:20.9%}.speedT_165{left:68.7%;top:21.5%}.speedT_170{left:70.7%;top:22.4%}.speedT_175{left:72.5%;top:23.3%}' +
    '.speedT_180{left:74.2%;top:24.5%}.speedT_185{left:75.8%;top:25.7%}.speedT_190{left:77.1%;top:27.1%}.speedT_195{left:78%;top:28.6%}.speedT_200{left:78.6%;top:30.3%}.speedT_205{left:78.9%;top:32%}.speedT_210{left:78.6%;top:33.7%}' +
    '.speedT_215{left:78%;top:35.3%}.speedT_220{left:77.1%;top:36.8%}.speedT_225{left:75.8%;top:38.2%}.speedT_230{left:74.2%;top:39.5%}.speedT_235{left:72.5%;top:40.6%}.speedT_240{left:70.7%;top:41.5%}.speedT_245{left:68.8%;top:42.4%}' +
    '.speedT_250{left:66.8%;top:43%}.speedT_255{left:64.8%;top:43.6%}.speedT_260{left:62.8%;top:44%}' +
    '</style></head><body><div class="container">' +
    '<div id="image-container"><img id="dynamic-image" src="/img" onerror="this.style.display=''none''"></div>' +
    '<div class="speed">000</div><div class="speedLimit">000</div>' +
    '<div class="tm">0.0</div><div class="ur">0.0</div>' +
    '<div class="alsen">x</div><div class="ek">EK</div>' +
    '<div class="np">0</div><div class="uscr">0.0</div>' +
    '<div class="coordinate"></div><div class="time">00:00:00</div>' +
    '<div class="distance">0000</div><div class="svetofor"></div>' +
    '<div class="als-white"></div><div class="als-red"></div>' +
    '<div class="als-yellowred"></div><div class="als-yellow"></div>' +
    '<div class="als-green"></div><div class="als-green2"></div>' +
    '<div class="als-green3"></div><div class="als-green4"></div>' +
    '<div class="signal"></div><div class="mode"></div>' +
    '<div class="station"></div><div class="als-rectangle"></div>' +
    '<div class="forward" style="display:none">' +
    '\u25B2</div>' +
    '<div class="backward" style="display:none">' +
    '\u25BC</div>' +
    '<div class="speedometer"></div></div>' +
    '<script>' +
    'var globalMaxSpeed=260;' +
    'function createSpeedometerDots(){' +
    'const maxSpeed=globalMaxSpeed,step=5;' +
    'const dot="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADwAAAA8CAYAAAA6/NlyAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAIGNIUk0AAHolAACAgwAA+f8AAIDpAAB1MAAA6mAAADqYAAAXb5JfxUYAAAD4SURBVHja7NoxDsFQAIfxr+IIBhIDS49gsJgsVjOLA4iTiAOYzFaLGziChcFgcIdankREpMujre8"' +
    '+"/NS9fmvySDm9okmUZ/7QafzbBggULFixYsGDBggULFixYsGDBggULFvzT1WO+fEXyeGwCM2AEdMPZCdgBa+AKMCcrNzisB2yA9OW8BfSBKTABDlX4pDvA9g32eWloOlUAL4B2jq4d2tKDh5HawoIbkdrCgm+R2sKC95HawoKXwCVHdwlt6cFnYAwcPzTH0JxLf9MKOwCDPDetby"' +
    '+"zxHw/BggULFixYsGDBggULFixYsGDBggULFvzF3QEAAP//AwCJJCI/FUNetAAAAABJRU5ErkJggg==";' +
    'const dotL="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADwAAAA8CAYAAAA6/NlyAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAIGNIUk0AAHolAACAgwAA+f8AAIDpAAB1MAAA6mAAADqYAAAXb5JfxUYAAADtSURBVHja7NotDsJAEIbhdwlHQCAQcIk6JAbbI3CCHohrYJCcojVUIBDcYTC1/ISwpcA7yZjm"' +
    '+"S7ZPNp1u0qaI4J9qxJ+VYMGCBQsWLFiwYMGCBQsWLFiwYMGCBQv+aI17WSWlKbAB1sCiu3oEdsCWiHNv4ojI21AE1AFxo+uAIvt9dJ2yfmpJaQ4cgNmD5AlYEtF++zNcPYGly1S/MLRWmbKDBU8yZQcLvmTKDha8z5R9fY46pd/7km+BEmjupBqg7AObf4cHeNJK/uMhWLBgwYIF"' +
    '+"CxYsWLBgwYIFCxYsWLBgwYJ7rCsAAAD//wMAqUvaDTsOLysAAAAASUVORK5CYII=";' +
    'const config=[' +
    '{type:"speed",className:"speed_m",containerClass:"speeds",imgSrc:dot},' +
    '{type:"speedL",className:"speedL_m",containerClass:"speedLimits",imgSrc:dotL},' +
    '{type:"speedT",className:"speedT_m",containerClass:"targetSpeeds",imgSrc:dot}' +
    '];' +
    'const sp=document.createElement("div");sp.className="speedometer";' +
    'config.forEach(({type,className,containerClass,imgSrc})=>{' +
    'const c=document.createElement("div");c.className=containerClass;' +
    'for(let s=0;s<=maxSpeed;s+=step){' +
    'const img=document.createElement("img");img.src=imgSrc;' +
    'img.alt=type+"_"+s;img.className=className+" "+type+"_"+s;' +
    'c.appendChild(img);}sp.appendChild(c);});return sp;}' +
    'document.querySelector(".container").appendChild(createSpeedometerDots());' +
    'function updateData(){fetch("/data").then(r=>r.json()).then(data=>{' +
    'var newMax=data.block160?160:260;' +
    'if(newMax!==globalMaxSpeed){globalMaxSpeed=newMax;' +
    'var oldSp=document.querySelector(".speedometer");' +
    'if(oldSp)oldSp.remove();' +
    'document.querySelector(".container").appendChild(createSpeedometerDots());}' +
    'let sE=document.querySelector(".speed");' +
    'let slE=document.querySelector(".speedLimit");' +
    'let rect=document.querySelector(".als-rectangle");' +
    'sE.textContent=data.speed;slE.textContent=data.speed_limit;' +
    'updateSpeedImages(Number(data.speed));' +
    'updateSpeedLimitImages(Number(data.speed_limit));' +
    'var np=document.querySelector(".np");' +
    'if(data.pp==0)np.textContent=data.np+"\u041D\u041F";' +
    'else np.textContent=data.np+"\u041F\u0420";' +
    'if(data.mode==0)updateSpeedTargetImages(data.target_speed);' +
    'else updateSpeedTargetImages(0);' +
    'document.querySelector(".signal").textContent=data.svetofor;' +
    'if(data.speed_limit==="000"){' +
    'sE.classList.add("blink");slE.style.display="none";' +
    'rect.style.display="none";}else{' +
    'sE.classList.remove("blink");slE.style.display="block";' +
    'rect.style.display="block";}' +
    'document.querySelector(".tm").textContent=' +
    '(parseFloat(data.tm_pressure)/10).toFixed(2);' +
    'document.querySelector(".ur").textContent=' +
    '(parseFloat(data.ur_pressure)/10).toFixed(2);' +
    'document.querySelector(".coordinate").textContent=data.coordinate;' +
    'document.querySelector(".time").textContent=data.time;' +
    'document.querySelector(".distance").textContent=data.distance;' +
    'document.querySelector(".uscr").textContent=data.acceleration;' +
    'document.querySelector(".station").textContent=data.station;' +
    'var fwd=document.querySelector(".forward");' +
    'var bwd=document.querySelector(".backward");' +
    'if(Number(data.speed)>0){' +
    'if(data.reverse=="FWD"){fwd.style.display="block";bwd.style.display="none";}' +
    'else{fwd.style.display="none";bwd.style.display="block";}}' +
    'else{fwd.style.display="none";bwd.style.display="none";}' +
    'if(data.np>0)document.querySelector(".ek").textContent="\u042D\u041A";' +
    'var g1=document.querySelector(".als-green");' +
    'var g2=document.querySelector(".als-green2");' +
    'var g3=document.querySelector(".als-green3");' +
    'var g4=document.querySelector(".als-green4");' +
    'g1.style.display="none";g2.style.display="none";' +
    'g3.style.display="none";g4.style.display="none";' +
    'var ae=document.querySelector(".alsen");' +
    'if(data.alsen>0){' +
    'ae.textContent=data.alsen+"\u0415\u041D";}' +
    'else{ae.textContent="x";}' +
    'if(data.als<=4){' +
    'if(data.als==1)rect.className="als-rectangle als-white";' +
    'else if(data.als==2)rect.className="als-rectangle als-red";' +
    'else if(data.als==3)' +
    'rect.className="als-rectangle als-yellowred";' +
    'else if(data.als==4)' +
    'rect.className="als-rectangle als-yellow";' +
    'else rect.className="als-rectangle als-white";' +
    '}else if(data.alsen>0){' +
    'if(data.block_uch==0){' +
    'rect.className="als-rectangle als-yellowred";}' +
    'else{rect.className="als-rectangle als-yellow";}' +
    'if(data.block_uch>=2)g1.style.display="block";' +
    'if(data.block_uch>=3)g2.style.display="block";' +
    'if(data.block_uch>=4)g3.style.display="block";' +
    'if(data.block_uch>=5)g4.style.display="block";' +
    '}else{' +
    'rect.className="als-rectangle";' +
    'g1.style.display="block";}' +
    'var mE=document.querySelector(".mode");' +
    'switch(data.mode){' +
    'case 2:mE.textContent="\u041F";' +
    'mE.classList.add("blink");mE.style.top="8.8%";break;' +
    'case 1:mE.textContent="\u041C";' +
    'mE.classList.remove("blink");mE.style.top="4.5%";break;' +
    'case 0:mE.textContent="\u041F";' +
    'mE.classList.remove("blink");mE.style.top="8.7%";break;' +
    'default:mE.textContent="";' +
    'mE.classList.remove("blink");break;}' +
    '}).catch(e=>console.error(e));}' +
    'setInterval(updateData,150);' +
    'function updateSpeedImages(speed){' +
    'document.querySelectorAll(".speed_m").forEach(' +
    'img=>img.style.display="none");' +
    'for(let i=0;i<=speed;i+=5){' +
    'let img=document.querySelector(".speed_"+i);' +
    'if(img)img.style.display="block";}}' +
    'function updateSpeedLimitImages(sl){' +
    'let r=Math.round(sl/5)*5;' +
    'document.querySelectorAll(".speedL_m").forEach(' +
    'img=>img.style.display="none");' +
    'let img=document.querySelector(".speedL_"+r);' +
    'if(img)img.style.display="block";}' +
    'function updateSpeedTargetImages(st){' +
    'let r=Math.round(st/5)*5;' +
    'document.querySelectorAll(".speedT_m").forEach(' +
    'img=>img.style.display="none");' +
    'let img=document.querySelector(".speedT_"+r);' +
    'if(img)img.style.display="block";}' +
    '</script></body></html>');
end;

procedure SendImageFile(ClientSock: Integer);
var
  F: file;
  ImgPath: string;
  FSize: Integer;
  Header: AnsiString;
  Buf: array[0..8191] of Byte;
  BytesRead: Integer;
begin
  try
  ImgPath := ExtractFilePath(ParamStr(0)) + 'bil_bg.jpg';
  if not FileExists(ImgPath) then
  begin
    Header := 'HTTP/1.1 404 Not Found'#13#10 +
      'Connection: close'#13#10 +
      'Content-Length: 0'#13#10#13#10;
    send(ClientSock, Header[1], Length(Header), 0);
    Exit;
  end;

  AssignFile(F, ImgPath);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then Exit;
  FSize := FileSize(F);
  CloseFile(F);

  Header := 'HTTP/1.1 200 OK'#13#10 +
    'Content-Type: image/jpeg'#13#10 +
    'Connection: close'#13#10 +
    'Cache-Control: max-age=86400'#13#10 +
    'Content-Length: ' + AnsiString(IntToStr(FSize)) + #13#10#13#10;
  send(ClientSock, Header[1], Length(Header), 0);

  AssignFile(F, ImgPath);
  Reset(F, 1);
  try
    repeat
      BlockRead(F, Buf, SizeOf(Buf), BytesRead);
      if BytesRead > 0 then
        send(ClientSock, Buf, BytesRead, 0);
    until BytesRead = 0;
  finally
    CloseFile(F);
  end;
  except
    // Ignore image serving errors
  end;
end;

function ServerThreadProc(Param: Pointer): DWORD; stdcall;
var
  WSAData: TWSAData;
  ServerAddr: TSockAddrIn;
  ClientSocket: Integer;
  ClientAddr: TSockAddrIn;
  AddrLen: Integer;
  RecvBuf: array[0..4095] of AnsiChar;
  BytesRecv: Integer;
  Request, Response, Body, Header: AnsiString;
  Path: AnsiString;
  SpacePos1, SpacePos2: Integer;
begin
  Result := 0;
  if WSAStartup($0202, WSAData) <> 0 then Exit;

  ServerSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if ServerSocket = INVALID_SOCKET then
  begin
    WSACleanup;
    Exit;
  end;

  ServerAddr.sin_family := AF_INET;
  ServerAddr.sin_port := htons(BilServerPort);
  ServerAddr.sin_addr.S_addr := INADDR_ANY;

  if bind(ServerSocket, ServerAddr, SizeOf(ServerAddr)) = SOCKET_ERROR then
  begin
    closesocket(ServerSocket);
    WSACleanup;
    Exit;
  end;

  if listen(ServerSocket, 5) = SOCKET_ERROR then
  begin
    closesocket(ServerSocket);
    WSACleanup;
    Exit;
  end;

  BilServerRunning := True;

  while not StopFlag do
  begin
    AddrLen := SizeOf(ClientAddr);
    ClientSocket := accept(ServerSocket, @ClientAddr, @AddrLen);
    if ClientSocket = INVALID_SOCKET then Continue;

    BytesRecv := recv(ClientSocket, RecvBuf, SizeOf(RecvBuf) - 1, 0);
    if BytesRecv > 0 then
    begin
      try
      RecvBuf[BytesRecv] := #0;
      Request := AnsiString(RecvBuf);

      SpacePos1 := Pos(' ', string(Request));
      if SpacePos1 > 0 then
      begin
        SpacePos2 := Pos(' ', string(Copy(Request, SpacePos1 + 1, Length(Request))));
        if SpacePos2 > 0 then
          Path := Copy(Request, SpacePos1 + 1, SpacePos2 - 1)
        else
          Path := '/';
      end
      else
        Path := '/';

      if Path = '/data' then
      begin
        Body := BuildJsonData;
        Header := 'HTTP/1.1 200 OK'#13#10 +
          'Content-Type: application/json; charset=utf-8'#13#10 +
          'Access-Control-Allow-Origin: *'#13#10 +
          'Connection: close'#13#10 +
          'Content-Length: ' + AnsiString(IntToStr(Length(Body))) + #13#10#13#10;
        Response := Header + Body;
        send(ClientSocket, Response[1], Length(Response), 0);
      end
      else if Path = '/img' then
      begin
        SendImageFile(ClientSocket);
      end
      else
      begin
        Body := GetBilHtml;
        Header := 'HTTP/1.1 200 OK'#13#10 +
          'Content-Type: text/html; charset=utf-8'#13#10 +
          'Connection: close'#13#10 +
          'Content-Length: ' + AnsiString(IntToStr(Length(Body))) + #13#10#13#10;
        Response := Header + Body;
        send(ClientSocket, Response[1], Length(Response), 0);
      end;
      except
        // Ignore request handling errors
      end;
    end;
    closesocket(ClientSocket);
  end;

  closesocket(ServerSocket);
  ServerSocket := -1;
  WSACleanup;
  BilServerRunning := False;
end;

procedure BilServer_Start;
var
  ThreadId: DWORD;
  WSAData: TWSAData;
begin
  if BilServerRunning then Exit;
  IsMultiThread := True;
  WSAStartup($0202, WSAData);
  CollectLocalIPs;
  WSACleanup;
  StopFlag := False;
  ServerThread := CreateThread(nil, 0, @ServerThreadProc, nil, 0, ThreadId);
end;

procedure BilServer_Stop;
begin
  if not BilServerRunning then Exit;
  StopFlag := True;
  if ServerSocket <> -1 then
    closesocket(ServerSocket);
  if ServerThread <> 0 then
  begin
    WaitForSingleObject(ServerThread, 3000);
    CloseHandle(ServerThread);
    ServerThread := 0;
  end;
  BilServerRunning := False;
end;

function BilServer_GetAddressCount: Integer;
begin
  if BilServerRunning then
    Result := BilServerIPCount
  else
    Result := 0;
end;

function BilServer_GetAddress(Index: Integer): string;
begin
  if BilServerRunning and (Index >= 0) and (Index < BilServerIPCount) then
    Result := 'http://' + BilServerIPs[Index] + ':' + IntToStr(BilServerPort)
  else
    Result := '';
end;

end.
