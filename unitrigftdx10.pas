unit UnitRigFTdx10;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  ExtCtrls,
  Dateutils,
  StrUtils,
  LazSynaser,
  Syncobjs,

  UnitRig,
  UnitSettings,

  UnitFormDebug;

type
  { state is internal, private object }
  TFTdx10rigState = class
    public
      constructor Create;

    private
      active: Boolean;
      vfoA_frq: Longword;
      vfoB_frq: Longword;
      vfoA_mode: String;
      vfoB_mode: String;
      vfo: Byte;
      split: Boolean;
      ptt: Boolean;
      frq: Longword;
      mode: String;
      pwr: Byte;
      vox: Boolean;
      dnr: Boolean;
      dnf: Boolean;
      drPortGain: Byte;
      txw: Boolean;
      meterValuePwr: Byte;
      meterValueSwr: Byte;
      meterValueS: Byte;
      fpwrMax: Byte;
      fpwrSum: Integer;
      fpwrCnt: Integer;
      rpwrSum: Integer;
      rpwrCnt: Integer;
      onEvent: TRigEvent;
      version: String;
  end;

  TFTdx10rigPayloadSuspend = class
    public
      constructor Create(waitFor: String; ignore: String);
      function match(payload: String): Boolean;
      function ignore(payload: String): Boolean;

    private
      wf: String;
      ig: String;
  end;

  TFTdx10rig = class(TInterfacedObject, TRig)
    public
      constructor Create(Config: TConfiguration);
      destructor Destroy; override;

      procedure setEventHandler(handler: TRigEvent);

      procedure Start;
      procedure Stop;
      procedure Send(payload: String);

      procedure SetVfoA_frq(frq: Longword);
      procedure SetVfoB_frq(frq: Longword);
      procedure SetVfoA_mode(mode: String);
      procedure SetVfoB_mode(mode: String);
      procedure SetSplit(active: Boolean);
      procedure SetPtt(active: Boolean);
      procedure SetCurrentVFOMode(mode: String);
      procedure SetPwr(pwr: Byte);
      procedure SetdrPortGain(gain: Byte);
      procedure toggleVox;
      procedure toggleDnr;
      procedure toggleDnf;
      procedure toggleTxw;
      procedure SetBand_160;
      procedure SetBand_80;
      procedure SetBand_60;
      procedure SetBand_40;
      procedure SetBand_30;
      procedure SetBand_20;
      procedure SetBand_17;
      procedure SetBand_15;
      procedure SetBand_12;
      procedure SetBand_10;
      procedure SetBand_6;

      function isActive:Boolean;
      function pttActive:Boolean;
      function splitActive:Boolean;
      function voxActive:Boolean;
      function dnrActive:Boolean;
      function dnfActive:Boolean;
      function txwActive:Boolean;
      function getPwr: Byte;
      function getVfoA_frq:Longword;
      function getVfoA_mode:String;
      function getVfoB_frq:Longword;
      function getVfoB_mode:String;
      function getVfo: Byte;
      function getMode:String;
      function getFrq:Longword;
      function getBand:Byte;
      function getdrPortGain: Byte;
      function getFPwrCur: Byte;
      function getFPwrMax: Byte;
      function getRPwrAvgPercent: Double;
      function getSMeter: String;
      function getVersion: String;

    private
      configuration: TConfiguration;
      rigState: TFTdx10rigState;
      comportMutex: TCriticalSection;
      comport: TBlockSerial;
      comportConnected: Boolean;
      timerKeepAlive: TTimer;
      timerRead: TTimer;
      ignorePayload: TFTdx10rigPayloadSuspend;
      comportLastCommTs: Longword;

    private
      procedure Connect(port: String);
      procedure Disconnect;
      procedure Read;
      procedure ProcessCmd(payload: String);
      procedure onTimerKeepAlive(Sender: TObject);
      procedure onTimerRead(Sender: TObject);
      procedure SetVfoMode(vfo: Byte; mode: String);
      function modeToStateMode(mode: String): String;
      function frqToBand(frq: Longword):Byte;
      function pwrCalc(input: Byte): Byte;
  end;

implementation

{ TRigPayloadSuspend }

constructor TFTdx10rigPayloadSuspend.Create(waitFor: String; ignore: String);
begin
  wf:=waitFor;
  ig:=ignore;
  FormDebug.Log('[Rig] suspending payload ' + ig + ' - waiting for ' + wf);
end;

function TFTdx10rigPayloadSuspend.match(payload: String): Boolean;
begin
  if (payload = wf) then Result:=True else Result:=False;
  if Result then FormDebug.Log('[Rig] suspending payload - release on ' + payload);
end;

function TFTdx10rigPayloadSuspend.ignore(payload: String): Boolean;
begin
  if StartsStr(ig, payload) then Result:=True else Result:=False;
end;

{ TFTdx10rigState }

constructor TFTdx10rigState.Create;
begin
   active:=False;
   ptt:=False;
   fpwrMax:=0;
   fpwrSum:=0;
   fpwrCnt:=0;
   rpwrSum:=0;
   rpwrCnt:=0;
end;

{ TFTdx10rig }

constructor TFTdx10rig.Create(Config: TConfiguration);
begin
  timerKeepAlive:=TTimer.Create(nil);
  timerKeepAlive.Enabled:=False;

  configuration:=Config;

  comportMutex:=TCriticalSection.Create;

  rigState:=TFTdx10rigState.Create;

  comportConnected:=False;
end;

procedure TFTdx10rig.setEventHandler(handler: TRigEvent);
begin
  rigState.onEvent:=handler;
end;

procedure TFTdx10rig.Start;
begin
  if not timerKeepAlive.Enabled then
  begin
    comportLastCommTs:=DateTimeToUnix(Now());

    timerKeepAlive.Interval:=1000;
    timerKeepAlive.OnTimer:=@onTimerKeepAlive;
    timerKeepAlive.Enabled:=True;
  end;
end;

procedure TFTdx10rig.onTimerKeepAlive(Sender: TObject);
var
  searchResult: TSearchRec;
  port: String;
  i: Integer = 0;
  autoDiscoverThreads: Array of TRigPortDiscoverThread;
  autoDiscoverThread : TRigPortDiscoverThread;
begin
  if not comportConnected then
  begin
    port:=Configuration.Settings.trxPort;
    if port='AUTO' then
    begin
      {$IFDEF DARWIN}
      if FindFirst('/dev/tty.SLAB_USBtoUART*', faAnyFile, searchResult) = 0 then
      begin
        repeat begin
          SetLength(autoDiscoverThreads, i + 1);
          autoDiscoverThreads[i]:=TRigPortDiscoverThread.Create(('/dev/' + searchResult.Name), Configuration.Settings.trxPortRate, 'ID;', 'ID0761');
          Inc(i);
        end
        until FindNext(searchResult) <> 0;
        FindClose(searchResult);
      end;
      for autoDiscoverThread in autoDiscoverThreads do begin
        autoDiscoverThread.WaitFor;
        if (autoDiscoverThread.getPort <> '') then port:=autoDiscoverThread.getPort;
      end;
      {$ENDIF}
    end;

    if port <> 'AUTO'
      then Connect(port)
      else FormDebug.Log('[Rig] port not discovered');
  end;

  if comportConnected and rigState.active and ((comportLastCommTs + 5) <  DateTimeToUnix(Now())) then Disconnect;
end;

procedure TFTdx10rig.Connect(port: String);
begin
  if  (port <> '') and not comportConnected then
  begin
    try
      comportMutex.Acquire;

      comport:=TBlockSerial.Create;
      comport.LinuxLock:=False;
      comport.NonBlock:=True;
      comport.Connect(port);
      comport.Config(Configuration.Settings.trxPortRate, 8, 'N', SB1, False, True);

      FormDebug.Log('[Rig] connection to ' + port +  ' -> ' + comport.LastErrorDesc + ' code: ' + Inttostr(comport.LastError));

      if comport.LastError = 0 then
      begin
        comportConnected:=True;

        comport.Purge;

        timerRead:=TTimer.Create(nil);
        timerRead.Interval:=configuration.Settings.trxPool;
        timerRead.OnTimer:=@onTimerRead;
        timerRead.Enabled:=True;
      end;
    finally
      comportMutex.Release;
    end;
  end;
end;

procedure TFTdx10rig.Send(payload: String);
begin
  if (comportConnected = true) then
  begin
    FormDebug.log('[Rig] send ' + payload);
    comportMutex.Acquire;
    try
      comport.SendString(Utf8ToAnsi(payload + ';'));
    finally
      comportMutex.Release;
    end;
  end;
end;

procedure TFTdx10rig.onTimerRead(Sender: TObject);
begin
  { send request for all data required for state update }
  if (comportConnected = true) then
  begin
    comportMutex.Acquire;
    try
      comport.SendString(Utf8ToAnsi('ID;TX;VS;FA;MD0;FB;MD1;ST;RM1;PC;VX;NR0;BC0;EX010417;TS;'));
      if rigState.ptt then comport.SendString(Utf8ToAnsi('RM5;RM6;'));
      Read;
    finally
      comportMutex.Release;
    end;
  end;

  { now can read data from TRX }
  Read;
end;

procedure TFTdx10rig.Read;
var
  r: AnsiString;
  payload: String;
begin
  if comportConnected then
  begin
    comportMutex.Acquire;
    try
      while comport.WaitingData > 0 do begin
        r:=comport.RecvTerminated(10, ';');
        payload:=Trim(AnsiToUtf8(r));
        if Length(payload) >= 3 then
        begin
          rigState.active:=True;
          comportLastCommTs:=DateTimeToUnix(Now());
          if Assigned(ignorePayload) and ignorePayload.match(payload) then begin
            FreeAndNil(ignorePayload);
            ProcessCmd(payload);
          end else if Assigned(ignorePayload) and (not ignorePayload.ignore(payload)) then begin
            ProcessCmd(payload);
          end else if not Assigned(ignorePayload) then begin
            ProcessCmd(payload);
          end;
        end;
      end;
    finally
      comportMutex.Release
    end;
  end;
end;

procedure TFTdx10rig.ProcessCmd(payload: String);
var
  command: String;
  meter: Char;
  meterValue: Integer;
  pttWas: Boolean;
  bandWas: Byte;
  modeWas: String;
begin
  bandWas:=frqToBand(rigState.frq);
  modeWas:=rigState.mode;

  command:=payload.Substring(0, 2);

  case command of
    'ID': rigState.version:=payload.Substring(2, 4);

    'EX': begin
      if (payload.Substring(2, 6) = '010417') then rigState.drPortGain:=StrToInt(payload.Substring(8, 3));
    end;

    'BC': rigState.dnf:=(payload.Substring(3, 1) = '1');

    'NR': rigState.dnr:=(payload.Substring(3, 1) = '1');

    'TS': rigState.txw:=(payload.Substring(2, 1) = '1');

    'VX': rigState.vox:=(payload.Substring(2, 1) = '1');

    'RM': begin
      meter:=payload[3];
      meterValue:=StrToInt(payload.Substring(3, 3));
      case meter of
        '1': rigState.meterValueS:=meterValue;
        '5': begin
          rigState.meterValuePwr:=meterValue;
          if rigState.fpwrMax < meterValue then rigState.fpwrMax:=meterValue;
          rigState.fpwrSum:=rigState.fpwrSum + meterValue;
          rigState.fpwrCnt:=rigState.fpwrCnt + 1;
        end;
        '6': begin
          rigState.meterValueSwr:=meterValue;
          rigState.rpwrSum:=rigState.rpwrSum + meterValue;
          rigState.rpwrCnt:=rigState.rpwrCnt + 1;
        end
      end;
    end;

    'PC': rigState.pwr:=StrToInt(payload.Substring(2, 3));

    'VS': rigState.vfo:=StrToInt(payload.Substring(2, 1));

    'MD': begin
      case payload.Substring(2, 1) of
        '0': rigState.vfoA_mode:=modeToStateMode(payload.Substring(3, 1));
        '1': rigState.vfoB_mode:=modeToStateMode(payload.Substring(3, 1));
      end;
      case rigState.vfo of
        0: rigState.mode:=rigState.vfoA_mode;
        1: rigState.mode:=rigState.vfoB_mode;
      end
    end;

    'FA': begin
      rigState.vfoA_frq:=StrToInt(payload.Substring(2, 9));
      if rigState.vfo = 0 then rigState.frq:=rigState.vfoA_frq;
    end;

    'FB': begin
      rigState.vfoB_frq:=StrToInt(payload.Substring(2, 9));
      if rigState.vfo = 1 then rigState.frq:=rigState.vfoB_frq;
    end;

    'ST': rigState.split:=(StrToInt(payload.Substring(2, 1)) > 0);

    'TX': begin
      pttWas:=rigState.ptt;
      rigState.ptt:=(StrToInt(payload.Substring(2, 1)) > 0); // 2 - mic 1 - USB
      if not pttWas and rigState.ptt then
      begin
        FormDebug.Log('[Rig] PTT ON');
        rigState.fpwrMax:=0;
        rigState.fpwrSum:=0;
        rigState.fpwrCnt:=0;
        rigState.rpwrSum:=0;
        rigState.rpwrCnt:=0;
      end;
      if pttWas and not rigState.ptt then
      begin
        FormDebug.Log('[Rig] PTT OFF');
      end
    end
  end;

  if Assigned(rigState.onEvent) and (frqToBand(rigState.frq) <> bandWas)
  then rigState.onEvent.call(TRigEvents.BAND_CHANGE);
  if Assigned(rigState.onEvent) and (rigState.mode <> modeWas)
     then rigState.onEvent.call(TRigEvents.MODE_CHANGE);

end;

procedure TFTdx10rig.Stop;
begin
  if rigState.ptt then SetPtt(False);
  timerKeepAlive.Enabled:=False;
  Disconnect;
  FormDebug.Log('[Rig] stopped');
end;

procedure TFTdx10rig.Disconnect;
begin
  if comportConnected then
    begin
      FormDebug.Log('[Rig] disconnecting');

      timerRead.Enabled:=False;
      FreeAndNil(timerRead);

      comportConnected:=False;
      rigState.active:=False;

      comport.CloseSocket;
    end;

  FreeAndNil(comport);
end;

destructor TFTdx10rig.Destroy;
begin
  timerKeepAlive.Enabled:=False;
  FreeAndNil(timerKeepAlive);

  FreeAndNil(comportMutex);
  FreeAndNil(rigState);

  inherited;
end;

{ update TRX state procedures }

procedure TFTdx10rig.SetVfoA_frq(frq: Longword);
var
  frequency: String;
begin
  // VFO-A frequency (there is no _, in cmd it's only for readibility) - 21.700 MHz -> 21_270_000 -> FA_021_270_000 (FA + 6 chars left padded with 0)
  frequency:=ReplaceStr(PadLeft(IntToStr(frq), 9), ' ', '0');
  ignorePayload:=TFTdx10rigPayloadSuspend.Create('FA' + frequency, 'FA');
  Send('FA' + frequency);
  rigState.vfoA_frq:=frq;
end;

procedure TFTdx10rig.SetVfoB_frq(frq: Longword);
var
  frequency: String;
begin
  frequency:=ReplaceStr(PadLeft(IntToStr(frq), 9), ' ', '0');
  Send('FB' + frequency);
end;

procedure TFTdx10rig.SetdrPortGain(gain: Byte);
var
  payload: String;
begin
  rigState.drPortGain:=gain;
  payload:='EX010417' + ReplaceStr(PadLeft(IntToStr(gain), 3), ' ', '0');
  ignorePayload:=TFTdx10rigPayloadSuspend.Create(payload, 'EX010417');
  Send(payload);
end;

procedure TFTdx10rig.SetCurrentVFOMode(mode: String);
begin
    SetVfoMode(rigState.vfo, mode);
end;

procedure TFTdx10rig.SetVfoA_Mode(mode: String);
begin
  SetVfoMode(0, mode);
end;

procedure TFTdx10rig.SetVfoB_Mode(mode: String);
begin
  SetVfoMode(1, mode);
end;

procedure TFTdx10rig.SetVfoMode(vfo: Byte; mode: String);
var
  translatedMode: String;
begin
  translatedMode:='';
  case mode of
   'LSB': translatedMode:='1';
   'USB': translatedMode:='2';
   'CW-U': translatedMode:='3';
   'FM': translatedMode:='4';
   'AM': translatedMode:='5';
   'RTTY-L': translatedMode:='6';
   'CW-L': translatedMode:='7';
   'DATA-L': translatedMode:='8';
   'RTTY-U': translatedMode:='9';
   'DATA-FM': translatedMode:='A';
   'FM-N': translatedMode:='B';
   'DATA-U': translatedMode:='C';
   'AM-N': translatedMode:='D';
   'PSK': translatedMode:='E';
   'DATA-FM-N': translatedMode:='F';
  end;
  if translatedMode <> '' then Send('MD' + IntToStr(vfo) + translatedMode);
end;

procedure TFTdx10rig.SetSplit(active: Boolean);
begin
  if active then begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('ST1', 'ST0');
    rigState.split:=True;
    Send('ST1');
  end else begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('ST0', 'ST1');
    rigState.split:=False;
    Send('ST0');
  end;
end;

procedure TFTdx10rig.SetPtt(active: Boolean);
begin
  if active then Send('TX1') else Send('TX0');
end;

procedure TFTdx10rig.SetPwr(pwr: Byte);
var
  payload: String;
begin
  rigState.pwr:=pwr;
  payload:='PC' + ReplaceStr(PadLeft(IntToStr(pwr), 3), ' ', '0');
  ignorePayload:=TFTdx10rigPayloadSuspend.Create(payload, 'PC');
  Send(payload);
end;

procedure TFTdx10rig.toggleVox;
begin
  if rigState.vox then begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('VX0', 'VX1');
    Send('VX0')
  end else begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('VX1', 'VX0');
    Send('VX1')
  end;
  rigState.vox:=(not rigState.vox);
end;

procedure TFTdx10rig.toggleDnr;
begin
    if rigState.dnr then begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('NR00', 'NR01');
    Send('NR00')
  end else begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('NR01', 'NR00');
    Send('NR01')
  end;
  rigState.dnr:=(not rigState.dnr);
end;

procedure TFTdx10rig.toggleDnf;
begin
  if rigState.dnf then begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('BC00', 'BC01');
    Send('BC00')
  end else begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('BC01', 'BC00');
    Send('BC01')
  end;
  rigState.dnf:=(not rigState.dnf);

  if rigState.dnf then Send(';') else Send(';');
end;

procedure TFTdx10rig.toggleTxw;
begin
    if rigState.txw then begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('TS0', 'TS1');
    Send('TS0')
  end else begin
    ignorePayload:=TFTdx10rigPayloadSuspend.Create('TS1', 'TS0');
    Send('TS1')
  end;
  rigState.dnr:=(not rigState.dnr);
end;

procedure TFTdx10rig.SetBand_160;
begin
  Send('BS00');
end;

procedure TFTdx10rig.SetBand_80;
begin
  Send('BS01');
end;

procedure TFTdx10rig.SetBand_60;
begin
  Send('BS02');
end;

procedure TFTdx10rig.SetBand_40;
begin
  Send('BS03');
end;

procedure TFTdx10rig.SetBand_30;
begin
  Send('BS04');
end;

procedure TFTdx10rig.SetBand_20;
begin
  Send('BS05');
end;

procedure TFTdx10rig.SetBand_17;
begin
  Send('BS06');
end;

procedure TFTdx10rig.SetBand_15;
begin
  Send('BS07');
end;

procedure TFTdx10rig.SetBand_12;
begin
  Send('BS08');
end;

procedure TFTdx10rig.SetBand_10;
begin
  Send('BS09');
end;

procedure TFTdx10rig.SetBand_6;
begin
  Send('BS10');
end;

function TFTdx10rig.isActive: Boolean;
begin
  Result:=rigState.active;
end;

function TFTdx10rig.pttActive: Boolean;
begin
  Result:=rigState.ptt;
end;

function TFTdx10rig.getPwr: Byte;
begin
  Result:=rigState.pwr;
end;

function TFTdx10rig.getVfoA_mode:String;
begin
  Result:=rigState.vfoA_mode;
end;

function TFTdx10rig.getVfoB_mode:String;
begin
  Result:=rigState.vfoB_mode;
end;

function TFTdx10rig.getVfo: Byte;
begin
  Result:=rigState.vfo;
end;

function TFTdx10rig.getMode:String;
begin
  Result:=rigState.mode;
end;

function TFTdx10rig.getBand:Byte;
begin
   frqToBand(rigState.frq)
end;

function TFTdx10rig.splitActive:Boolean;
begin
  Result:=rigState.split;
end;

function TFTdx10rig.getVfoA_frq:Longword;
begin
  Result:=rigState.vfoA_frq;
end;

function TFTdx10rig.getVfoB_frq:Longword;
begin
  Result:=rigState.vfoB_frq;
end;

function TFTdx10rig.getFrq:Longword;
begin
  Result:=rigState.frq;
end;

function TFTdx10rig.voxActive:Boolean;
begin
  Result:=rigState.vox;
end;

function TFTdx10rig.dnrActive:Boolean;
begin
  Result:=rigState.dnr;
end;

function TFTdx10rig.dnfActive:Boolean;
begin
  Result:=rigState.dnf;
end;

function TFTdx10rig.txwActive:Boolean;
begin
  Result:=rigState.txw;
end;

function TFTdx10rig.getdrPortGain: Byte;
begin
  Result:=rigState.drPortGain;
end;

function TFTdx10rig.getFPwrCur: Byte;
begin
   Result:=pwrCalc(rigState.meterValuePwr)
end;

function TFTdx10rig.getFPwrMax: Byte;
begin
   Result:=pwrCalc(rigState.fpwrMax)
end;

function TFTdx10rig.getRPwrAvgPercent:Double;
begin
  if (rigState.fpwrSum > 0) and (rigState.rpwrSum > 0)
    then Result:=(((rigState.rpwrSum*0.1)/rigState.rpwrCnt)/(rigState.fpwrSum/rigState.fpwrCnt))*100
    else Result:=0;
end;

function TFTdx10rig.getSMeter: String;
var
  input: Integer;
begin
  input:=rigState.meterValueS;
  if input <= 5 then Result:='0'
  else if input <= 10 then Result:='1'
  else if input <= 30 then Result:='2'
  else if input <= 50 then Result:='3'
  else if input <= 65 then Result:='4'
  else if input <= 80 then Result:='5'
  else if input <= 100 then Result:='6'
  else if input <= 115 then Result:='7'
  else if input <= 130 then Result:='8'
  else if input <= 150 then Result:='9'
  else if input <= 170 then Result:='9+10'
  else if input <= 255 then Result:='9+20';
end;

function TFTdx10rig.getVersion: String;
begin
  Result:='FTdx10 (' + rigState.version + ')';
end;

{ private helpers }

function TFTdx10rig.modeToStateMode(mode: String): String;
begin
   case mode of
   '1': Result:='LSB';
   '2': Result:='USB';
   '3': Result:='CW-U';
   '4': Result:='FM';
   '5': Result:='AM';
   '6': Result:='RTTY-L';
   '7': Result:='CW-L';
   '8': Result:='DATA-L';
   '9': Result:='RTTY-U';
   'A': Result:='DATA-FM';
   'B': Result:='FM-N';
   'C': Result:='DATA-U';
   'D': Result:='AM-N';
   'E': Result:='PSK';
   'F': Result:='DATA-FM-N';
   end;
end;

function TFTdx10rig.frqToBand(frq: Longword): Byte;
var
  mHz: Integer;
begin
  mHz:=Round(frq/1000000);
  if mHz <= 2 then Result:=160
  else if mHz <= 4 then Result:=80
  else if mHz <= 5 then Result:=60
  else if mHz <= 7 then Result:=40
  else if mHz <= 10 then Result:=30
  else if mHz <= 14 then Result:=20
  else if mHz <= 18 then Result:=17
  else if mHz <= 21 then Result:=15
  else if mHz <= 24 then Result:=12
  else if mHz <= 28 then Result:=10
  else if mHz <= 50 then Result:=6
  else Result:=0;
end;

function TFTdx10rig.pwrCalc(input: Byte): Byte;
begin
  Result:=0;
  if input > 0 then
  begin
    if input < 56 then Result:=5
    else if input < 100 then Result:=Round((((input*0.49) - 20)))
    else if input <= 150 then Result:=Round((input*0.5) - 25)
    else Result:=(input-100);
  end;
end;

end.

