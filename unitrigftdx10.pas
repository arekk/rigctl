unit UnitRigFTdx10;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  ExtCtrls,
  StrUtils,
  LazSynaser,
  Syncobjs,
  UnitEventsInterface,
  UnitSerialPortAutoDiscover,
  UnitRig,
  UnitSettings,
  UnitFormDebug;

type
  { state is internal, private object }
  TFTdx10rigState = class
    public
      constructor Create;
      procedure Reset;
    private
      active: Boolean;
      vfoA_frq: Longword;
      vfoB_frq: Longword;
      vfoA_mode: String;
      vfoB_mode: String;
      vfo: TRigVFO;
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
    private
      running: Boolean;
      autoDiscover: TSerialPortDiscover;
      autoDiscoverLockFile: String;
      rigState: TFTdx10rigState;
      comportMutex: TCriticalSection;
      timerKeepAlive: TTimer;
      comportConnected: Boolean;
      comport: TBlockSerial;
      timerRead: TTimer;
      ignorePayload: TFTdx10rigPayloadSuspend;
      keepAliveCounter: Integer;
      fOnEvent: IEvent;
      fOnState: TNotifyEvent;
    private
      procedure Connect(port: String);
      procedure Disconnect;
      procedure Read;
      procedure ProcessCmd(payload: String);
      procedure onTimerKeepAlive(Sender: TObject);
      procedure onPortDiscover(Sender: TObject);
      procedure onPortDiscoverFinish(Sender: TObject);
      procedure onTimerRead(Sender: TObject);
      procedure SetVfoMode(vfo: TRigVFO; mode: String);
      function modeToStateMode(mode: String): String;
      function frqToBand(frq: Longword):Byte;
      function pwrCalc(input: Byte): Byte;
    protected
      configuration: TConfiguration;
    protected
      function getSerialPortDiscoverParams: TSerialPortDiscoverParams; virtual;
      procedure Send(payload: String);
    public
      constructor Create(Config: TConfiguration);
      destructor Destroy; override;
      procedure Start;
      procedure Stop;
      procedure SetVfoA_frq(frq: Longword);
      procedure SetVfoB_frq(frq: Longword);
      procedure SetVfoA_mode(mode: String);
      procedure SetVfoB_mode(mode: String);
      procedure SetSplit(active: Boolean); virtual;
      procedure SetPtt(active: Boolean);
      procedure SetCurrentVFOMode(mode: String);
      procedure SetPwr(pwr: Byte);
      procedure SetdrPortGain(gain: Byte); virtual;
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
      procedure playMessage(no: Byte);
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
      function getVfo: TRigVFO;
      function getMode:String;
      function getFrq:Longword;
      function getBand:Byte;
      function getdrPortGain: Byte;
      function getFPwrCur: Byte;
      function getFPwrMax: Byte;
      function getRPwrAvgPercent: Double;
      function getSMeter: String;
      function getVersion: String;
      procedure onEvent(event: IEvent);
      procedure onState(event: TNotifyEvent);
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
  Reset;
end;

procedure TFTdx10rigState.Reset;
begin
   active:=False;
   ptt:=False;
   vfoA_frq:=0;
   vfoB_frq:=0;
   frq:=0;
   vfo:=RigVFO_A;
   fpwrMax:=0;
   fpwrSum:=0;
   fpwrCnt:=0;
   rpwrSum:=0;
   rpwrCnt:=0;
end;

{ TFTdx10rig }

{ core }

constructor TFTdx10rig.Create(Config: TConfiguration);
begin
  configuration:=Config;

  autoDiscover:=TSerialPortDiscover.Create(configuration.getConfigDirectory);
  autoDiscoverLockFile:='';

  rigState:=TFTdx10rigState.Create;

  comportMutex:=TCriticalSection.Create;

  keepAliveCounter:=0;

  timerKeepAlive:=TTimer.Create(nil);
  timerKeepAlive.Enabled:=False;

  comportConnected:=False;
end;

procedure TFTdx10rig.Start;
begin
  running:=True;

  rigState.Reset;
  if Assigned(fOnState) then fOnState(nil);

  timerKeepAlive.Interval:=2000;
  timerKeepAlive.OnTimer:=@onTimerKeepAlive;
  timerKeepAlive.Enabled:=True;
end;

function TFTdx10rig.getSerialPortDiscoverParams: TSerialPortDiscoverParams;
begin
  Result:=TSerialPortDiscoverParams.Create(TSettingsTrx.FTDX10, Configuration.Settings.trxPortRate, Utf8ToAnsi(';'), Utf8ToAnsi('ID'), Utf8ToAnsi('ID0761'), 50)
end;

procedure TFTdx10rig.onTimerKeepAlive(Sender: TObject);
var
  autoDiscoverThreads: Integer;
begin
  if not comportConnected then
  begin
    if configuration.Settings.trxPort = 'AUTO' then
    begin
      autoDiscover.OnSuccess:=@onPortDiscover;
      autoDiscover.OnFinish:=@onPortDiscoverFinish;
      autoDiscoverThreads:=autoDiscover.Discover(getSerialPortDiscoverParams);
      if autoDiscoverThreads > 0 then
      begin
        FormDebug.Log(Format('[Rig] disabling keep-alive and waiting for %d auto-discover threads to finish', [autoDiscoverThreads]));
        timerKeepAlive.Enabled:=False;
      end;
    end
    else
    begin
      autoDiscoverLockFile:=autoDiscover.LockPort(configuration.Settings.trxPort);
      Connect(configuration.Settings.trxPort);
    end;
  end;

  FormDebug.Log(Format('[Rig] connection: %s [%d]', [BoolToStr(comportConnected, True), keepAliveCounter]));

  if running and comportConnected then Inc(keepAliveCounter);
  if running and (keepAliveCounter > 5) then Disconnect;
end;

procedure TFTdx10rig.onPortDiscover(Sender: TObject);
begin
  autoDiscoverLockFile:=TSerialPortDiscoverResult(Sender).LockFile;
  if running then Connect(TSerialPortDiscoverResult(Sender).port);
end;

procedure TFTdx10rig.onPortDiscoverFinish(Sender: TObject);
begin
  FormDebug.Log(Format('[Rig] auto-discover threads finshed, setting keep-alive to %s', [BoolToStr(running, True)]));
  timerKeepAlive.Enabled:=running;
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

      FormDebug.Log('[Rig] connection to ' + comport.Device +  ' -> ' + comport.LastErrorDesc + ' code: ' + Inttostr(comport.LastError));

      if comport.LastError = 0 then
      begin
        comportConnected:=True;
        keepAliveCounter:=0;

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
    FormDebug.log('[Rig] < ' + payload);
    comportMutex.Acquire;
    try
      comport.SendString(Utf8ToAnsi(payload + ';'));
      comport.Flush;
    finally
      comportMutex.Release;
    end;
  end;
end;

procedure TFTdx10rig.onTimerRead(Sender: TObject);
begin
  { send request for all data required for state update }
  if comportConnected then
  begin
    comportMutex.Acquire;
    try
      comport.SendString(Utf8ToAnsi('ID;TX;VS;FA;MD0;FB;MD1;ST;RM1;PC;VX;NR0;BC0;EX010417;TS;'));
      if rigState.ptt then comport.SendString(Utf8ToAnsi('RM5;RM6;'));
    finally
      comportMutex.Release;
    end;
  end;

  { read data from TRX and update internal state object }
  Read;

  { call procedure to refllect state updates outside of unit }
  if Assigned(fOnState) then fOnState(nil);
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
          // any valid data incoming from port setting Rig in statr active
          rigState.active:=True;
          if keepAliveCounter > 0 then Dec(keepAliveCounter);

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
  pttWas:=rigState.ptt;
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

    'VS': begin
      case payload.Substring(2, 1) of
        '0': rigState.vfo:=RigVFO_A;
        '1': rigState.vfo:=RigVFO_B;
      end;
    end;

    'MD': begin
      case payload.Substring(2, 1) of
        '0': rigState.vfoA_mode:=modeToStateMode(payload.Substring(3, 1));
        '1': rigState.vfoB_mode:=modeToStateMode(payload.Substring(3, 1));
      end;
      case rigState.vfo of
        RigVFO_A: rigState.mode:=rigState.vfoA_mode;
        RigVFO_B: rigState.mode:=rigState.vfoB_mode;
      end
    end;

    'FA': begin
      rigState.vfoA_frq:=StrToInt(payload.Substring(2, 9));
      if rigState.vfo = RigVFO_A then rigState.frq:=rigState.vfoA_frq;
    end;

    'FB': begin
      rigState.vfoB_frq:=StrToInt(payload.Substring(2, 9));
      if rigState.vfo = RigVFO_B then rigState.frq:=rigState.vfoB_frq;
    end;

    'ST': rigState.split:=(StrToInt(payload.Substring(2, 1)) > 0);

    'TX': begin
      rigState.ptt:=(StrToInt(payload.Substring(2, 1)) > 0); // 2 - mic 1 - USB
      if not pttWas and rigState.ptt then
      begin
        if Assigned(fOnEvent) then fOnEvent.call(RigEventTxStart);
        rigState.fpwrMax:=0;
        rigState.fpwrSum:=0;
        rigState.fpwrCnt:=0;
        rigState.rpwrSum:=0;
        rigState.rpwrCnt:=0;
      end;
      if pttWas and not rigState.ptt then
      begin
        if Assigned(fOnEvent) then fOnEvent.call(RigEventTxStop);
      end
    end
  end;

  if Assigned(fOnEvent) then
  begin
    if frqToBand(rigState.frq) <> bandWas
       then fOnEvent.call(RigEventBandChange);

    if rigState.mode <> modeWas
       then fOnEvent.call(RigEventModeChange);
  end;

end;

procedure TFTdx10rig.Stop;
begin
  running:=False;
  timerKeepAlive.Enabled:=False;
  autoDiscover.Terminate;
  Disconnect;
  FormDebug.Log('[Rig] stopped');
end;

procedure TFTdx10rig.Disconnect;
begin
  if comportConnected then
  begin
    FormDebug.Log('[Rig] disconnecting');

    comportConnected:=False;

    rigState.Reset;
    if Assigned(fOnState) then fOnState(nil);

    timerRead.Enabled:=False;
    timerRead.Free;

    comport.Free;
  end;

  if (autoDiscoverLockFile <> '') and FileExists(autoDiscoverLockFile) then DeleteFile(autoDiscoverLockFile);
  autoDiscoverLockFile:='';
end;

destructor TFTdx10rig.Destroy;
begin
  timerKeepAlive.Enabled:=False;
  FreeAndNil(timerKeepAlive);

  FreeAndNil(comportMutex);
  FreeAndNil(rigState);
  FreeAndNil(autoDiscover);

  inherited;
end;

{ set/get }

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
  SetVfoMode(RigVFO_A, mode);
end;

procedure TFTdx10rig.SetVfoB_Mode(mode: String);
begin
  SetVfoMode(RigVFO_B, mode);
end;

procedure TFTdx10rig.SetVfoMode(vfo: TRigVFO; mode: String);
var
  translatedMode: String;
  translatedVfo: String;
begin
  case vfo of
    RigVFO_A: translatedVfo:='0';
    RigVFO_B: translatedVfo:='1';
  end;
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
  if translatedMode <> '' then Send('MD' + translatedVfo + translatedMode);
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

procedure TFTdx10rig.playMessage(no: Byte);
begin
  Send('BI1');
  Send('PB0' + IntToStr(no));
end;

function TFTdx10rig.isActive: Boolean;
begin
  Result:=(running and comportConnected and rigState.active);
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

function TFTdx10rig.getVfo: TRigVFO;
begin
  Result:=rigState.vfo;
end;

function TFTdx10rig.getMode:String;
begin
  Result:=rigState.mode;
end;

function TFTdx10rig.getBand:Byte;
begin
   Result:=frqToBand(rigState.frq)
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

procedure TFTdx10rig.onEvent(event: IEvent);
begin
  fOnEvent:=event;
end;

procedure TFTdx10rig.onState(event: TNotifyEvent);
begin
  fOnState:=event;
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
  mHz:=Trunc(frq/1000000);
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

