unit UnitSpert;

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
  UnitSettings,
  UnitFormDebug;

type
  TSpertFanLevel = (SpertFanLevel_Undef, SpertFanLevel_50, SpertFanLevel_Max, SpertFanLevel_Histeresis, SpertFanLevel_Fixed);

  TSpertLC = Array[1..8] of Double;

  TSpertState = class
    public
      constructor Create;
      procedure Reset;
    private
      active: Boolean;
      pwr: Word;
      band: String;
      temp:Word;
      tx: Boolean;
      fpwr: Integer;
      rpwr: Integer;
      atuPresent: Boolean;
      atuActive: Boolean;
      atuTune: Boolean;
      atuLastC: String;
      atuLastL: String;
      atuLastK: Byte;
      fan: TSpertFanLevel;
      version: String;
      txLocked: Boolean;
      fpwrMax: Integer;
      fpwrSum: Integer;
      fpwrCnt: Integer;
      rpwrMax: Integer;
      rpwrSum: Integer;
      rpwrCnt: Integer;
  end;

  TSpertPayloadSuspend = class
    waitFor: String;
    ignore: String;
  end;

  TSpert = class
    const
      COMPORT_DELIM = AnsiString(#13) + AnsiString(#10);
      ATU_L: TSpertLC = (5.12, 2.56, 1.28, 0.64, 0.32, 0.16, 0.08, 0.04);
      ATU_C: TSpertLC = (640, 320, 160, 80, 40, 20, 10, 5);
    private
      running: Boolean;
      configuration: TConfiguration;
      autoDiscover: TSerialPortDiscover;
      autoDiscoverLockFile: String;
      spertState: TSpertState;
      comportMutex: TCriticalSection;
      timerKeepAlive: TTimer;
      comportConnected: Boolean;
      comport: TBlockSerial;
      keepAliveCounter: Integer;
      timerRead: TTimer;
      ignorePayload: TSpertPayloadSuspend;
      fOnEvent: IEvent;
      fOnState: TNotifyEvent;
    private
      procedure onTimerKeepAlive(Sender: TObject);
      procedure onPortDiscover(Sender: TObject);
      procedure onPortDiscoverFinish(Sender: TObject);
      procedure Connect(port: String);
      procedure onTimerRead(Sender: TObject);
      procedure Read;
      procedure Send(cmd: String);
      procedure ProcessCmd(cmd: String);
      function extractInt(cmd: String; prefix: String): Integer;
      function calcAtu_LC(Input: String; Arr: TSpertLC): Double;
      procedure Disconnect;
    public
      constructor Create(Config: TConfiguration);
      destructor Destroy; override;
      procedure Start;
      procedure Stop;
      procedure toggleAtuState;
      procedure txLockOn;
      procedure txLockOff;
      procedure setPower(pwr: Integer);
      procedure tuneAtu;
      function isActive:Boolean;
      function getPwr: Word;
      function getBand: String;
      function getTemp:Word;
      function txActive:Boolean;
      function getFpwr: Integer; // reported forwarded power
      function getFpwrMax: Integer;
      function getFpwrAvg: Integer;
      function getRpwr: Integer; // reported reflected power
      function getRpwrMax: Integer;
      function getRpwrMaxPercent: Double;
      function isTxLocked: Boolean;
      function isAtuPresent: Boolean;
      function isAtuActive: Boolean;
      function getAtu_L: Double;
      function getAtu_C: Double;
      function getAtu_K: Byte;
      function isAtuTunning: Boolean;
      function getVersion: String;
      function getFan: TSpertFanLevel;
      procedure setFan(fan: TSpertFanLevel);
      procedure resetAtuMem;
      procedure onEvent(event: IEvent);
      procedure onState(event: TNotifyEvent);
  end;

implementation


{ TSpertState }

constructor TSpertState.Create;
begin
  Reset;
end;

procedure TSpertState.Reset;
begin
  active:=False;
  pwr:=0;
  band:='';
  temp:=0;
  tx:=False;
  fpwr:=0;
  fpwrMax:=0;
  fpwrSum:=0;
  fpwrCnt:=0;
  rpwr:=0;
  rpwrMax:=0;
  rpwrSum:=0;
  rpwrCnt:=0;
  atuPresent:=False;
  atuActive:=False;
  atuTune:=False;
  atuLastL:='';
  atuLastC:='';
  atuLastK:=0;
  version:='';
  fan:=SpertFanLevel_Undef;
  txLocked:=False;
end;

{ TSpert }

{ core }

constructor TSpert.Create(Config: TConfiguration);
begin
  configuration:=Config;

  autoDiscover:=TSerialPortDiscover.Create(configuration.getConfigDirectory);
  autoDiscoverLockFile:='';

  spertState:=TSpertState.Create;
  spertState.active:=False;

  comportMutex:=TCriticalSection.Create;

  keepAliveCounter:=0;

  timerKeepAlive:=TTimer.Create(nil);
  timerKeepAlive.Enabled:=False;

  comportConnected:=False;
end;

procedure TSpert.Start;
begin
  running:=True;

  spertState.Reset;
  if Assigned(fOnState) then fOnState(nil);

  timerKeepAlive:=TTimer.Create(nil);
  timerKeepAlive.Interval:=2000;
  timerKeepAlive.OnTimer:=@onTimerKeepAlive;
  timerKeepAlive.Enabled:=True;
end;

procedure TSpert.onTimerKeepAlive(Sender: TObject);
begin
  if not comportConnected then begin
    if configuration.Settings.spertPort = 'AUTO' then
    begin
      autoDiscover.OnSuccess:=@onPortDiscover;
      autoDiscover.OnFinish:=@onPortDiscoverFinish;
      if autoDiscover.Discover(TSerialPortDiscoverParams.Create('SPert', Configuration.Settings.spertPortRate, COMPORT_DELIM, Utf8ToAnsi('PA?'), Utf8ToAnsi('PA_SP7SP'), 150)) > 0
        then timerKeepAlive.Enabled:=False;
    end
    else
    begin
      autoDiscoverLockFile:=autoDiscover.LockPort(configuration.Settings.spertPort);
      Connect(configuration.Settings.spertPort);
    end;
  end;

  FormDebug.Log(Format('[Spert] connection: %s [%d]', [BoolToStr(comportConnected, True), keepAliveCounter]));

  // ping every 1s to check is connection still alive
  if running and comportConnected then
  begin
    Send('PA?');
    Inc(keepAliveCounter)
  end;

  if running and (keepAliveCounter > 5) then Disconnect;
end;

procedure TSpert.onPortDiscover(Sender: TObject);
begin
  autoDiscoverLockFile:=TSerialPortDiscoverResult(Sender).LockFile;
  if running then Connect(TSerialPortDiscoverResult(Sender).port);
end;

procedure TSpert.onPortDiscoverFinish(Sender: TObject);
begin
  timerKeepAlive.Enabled:=running;
end;

procedure TSpert.Connect(port: String);
begin
  if  (port <> '') and not comportConnected then
  begin
    comportMutex.Acquire;
    try
      FormDebug.Log('[Spert] connecting to ' + port + ' at ' + IntToStr(Configuration.Settings.spertPortRate));

      comport:=TBlockSerial.Create;
      comport.LinuxLock:=False;
      comport.NonBlock:=True;
      comport.Connect(port);
      comport.Config(Configuration.Settings.spertPortRate, 8, 'N', SB1, False, True);

      FormDebug.Log('[Spert] connetion to ' + comport.Device +  ' -> ' + comport.LastErrorDesc + ' code: ' + Inttostr(comport.LastError));

      if comport.LastError = 0 then
      begin
        comportConnected:=true;
        keepAliveCounter:=0;

        { main timer handling periodic read from SPert serial port }
        timerRead:=TTimer.Create(nil);
        timerRead.Interval:=Configuration.Settings.spertPool;
        timerRead.OnTimer:=@onTimerRead;
        timerRead.Enabled:=True;

        // set fan if required
        if (Configuration.Settings.spertStartupFan > 0) and (Configuration.Settings.spertStartupFan <= 4)
          then comport.SendString(Utf8ToAnsi('F: ' + IntToStr(Configuration.Settings.spertStartupFan)) + COMPORT_DELIM);

        // just after connect query current state
        comport.SendString(Utf8ToAnsi('???') + COMPORT_DELIM + Utf8ToAnsi('ATU?') + COMPORT_DELIM + Utf8ToAnsi('Port_L ?') + COMPORT_DELIM + Utf8ToAnsi('Port_C ?') + COMPORT_DELIM + Utf8ToAnsi('pin_K ?') + COMPORT_DELIM);
        comport.Flush;
        Sleep(500);

      end;
    finally
       comportMutex.Release;
    end;
  end;
end;

procedure TSpert.onTimerRead(Sender: TObject);
begin
  { read data from Spert and update internal state object, we don't need to ask about current status of amplifier because device will send to port any update made physically }
  Read;

  { call procedure to refllect state updates outside of unit }
  if Assigned(fOnState) then fOnState(nil);
end;

procedure TSpert.Read;
var
  payload: String;
  r: AnsiString;
begin
  if comportConnected then
  begin
    comportMutex.Acquire;
    try
      while comport.WaitingData > 0 do begin
        try
          r:=comport.RecvTerminated(10, COMPORT_DELIM);
          payload:=Trim(AnsiToUtf8(r));
          if payload <> '' then begin
            if payload <> 'PA_SP7SP' then FormDebug.Log('[Spert] > ' + payload);

            spertState.active:=True;
            if keepAliveCounter > 0 then Dec(keepAliveCounter);

            if Assigned(ignorePayload) and (payload = ignorePayload.waitFor) then begin
              FreeAndNil(ignorePayload);
              ProcessCmd(payload);
            end else if Assigned(ignorePayload) and (not StartsStr(ignorePayload.ignore, payload)) then begin
              ProcessCmd(payload);
            end else if not Assigned(ignorePayload) then begin
              ProcessCmd(payload);
            end;

          end;
        except
          on E: Exception do FormDebug.Log('[Spert] comport read exception - ' + E.Message);
        end;
      end;
    finally
      comportMutex.Release
    end;
  end;
end;

procedure TSpert.ProcessCmd(cmd: String);
var
  value: String;
  number: Integer;
begin
  // power
  if StartsStr('ALC_PWM:', cmd) then
  begin
    number:=extractInt(cmd, 'ALC_PWM: ');
    if number > -1 then spertState.pwr:=number;
  end;

  // band
  if StartsStr('B:', cmd) then
  begin
    value:=ReplaceText(cmd, 'B: ', '');
    if value='1' then spertState.band:=' 160M';
    if value='2' then spertState.band:=' 80M';
    if value='3' then spertState.band:=' 40M';
    if value='4' then spertState.band:=' 30M/20M';
    if value='5' then spertState.band:=' 17M/15M';
    if value='6' then spertState.band:=' 12M/10M';
    if value='7' then spertState.band:=' 6M';
  end;

  // temperature
  if StartsStr('T:', cmd) then
  begin
   number:= extractInt(cmd, 'T: ');
   if number > -1 then spertState.temp:=number;
  end;

  // forwarded power
  if StartsStr('O:', cmd) then
  begin
    number:= extractInt(cmd, 'O: ');
    if (number <= 1200) then
    begin
      spertState.fpwr:=number;

      if (number > spertState.fpwrMax) then spertState.fpwrMax:=number;
      spertState.fpwrSum:=spertState.fpwrSum + number;
      Inc(spertState.fpwrCnt);
    end;
  end;

  // reflected power - showing average
  if StartsStr('R:', cmd) then
  begin
    number:= extractInt(cmd, 'R: ');
    if (number <= 120) then
    begin
      spertState.rpwr:=number;

      if (number > spertState.rpwrMax) then spertState.rpwrMax:=number;
      spertState.rpwrSum:=spertState.rpwrSum + number;
      Inc(spertState.rpwrCnt);
    end;
  end;

  // PTT on/off
  if StartsStr('TX:', cmd) then
  begin
    value:=ReplaceText(cmd, 'TX: ', '');
    if value='ON' then
    begin
      spertState.tx:=True;
      spertState.fpwr:=0;
      spertState.fpwrMax:=0;
      spertState.fpwrSum:=0;
      spertState.fpwrCnt:=0;
      spertState.rpwr:=0;
      spertState.rpwrMax:=0;
      spertState.rpwrSum:=0;
      spertState.rpwrCnt:=0;
      if Assigned(fOnEvent) then fOnEvent.call(SpertEventTxStart);
    end;
    if value='OFF' then
    begin
      spertState.tx:=False;
      if Assigned(fOnEvent) then FOnEvent.call(SpertEventTxStop);
    end;
  end;

  // ATU presence (intalled or not)
  if StartsStr('ATU_INST:', cmd) then
  begin
    value:=ReplaceText(cmd, 'ATU_INST: ', '');
    spertState.atuPresent:=(value = '1');
  end;

  // ATU by-pass/activate (it use two separate commands)
  if StartsStr('ATU_STATE:', cmd) or StartsStr('ATU:', cmd) then
  begin
    if StartsStr('ATU_STATE:', cmd) then value:=ReplaceText(cmd, 'ATU_STATE: ', '');
    if StartsStr('ATU:', cmd) then value:=ReplaceText(cmd, 'ATU: ', '');
    spertState.atuActive:=(value = '1');
  end;

  // ATU tuning started
  if cmd='TUNE' then
  begin
    spertState.atuTune:=True;
    spertState.tx:=True;
  end;

  // ATU tuning finished
  if cmd='TUNE OFF' then
  begin
    spertState.atuTune:=False;
    spertState.tx:=False;
    if Assigned(fOnEvent) then fOnEvent.call(SpertEventAtferTuneStop);
  end;

  // ATU L/C/K configuration after tuning
  if StartsStr('END_L:', cmd) then spertState.atuLastL:=Trim(ReplaceText(cmd, 'END_L:', ''));
  if StartsStr('Port L:', cmd) then spertState.atuLastL:=Trim(ReplaceText(cmd, 'Port L:', ''));

  if StartsStr('END_C:', cmd) then spertState.atuLastC:=Trim(ReplaceText(cmd, 'END_C:', ''));
  if StartsStr('Port C:', cmd) then spertState.atuLastC:=Trim(ReplaceText(cmd, 'Port C:', ''));

  if StartsStr('END_K:', cmd) then spertState.atuLastK:=StrToInt(Trim(ReplaceText(cmd, 'END_K:', '')));
  if StartsStr('K:', cmd) then spertState.atuLastK:=StrToInt(Trim(ReplaceText(cmd, 'K:', '')));

  // firmware version
  if StartsStr('Ver:', cmd) then spertState.version:='SPert 1000 (' + ReplaceText(cmd, 'Ver: ', '') + ')';

  // Fan
  if StartsStr('F:', cmd) then begin
    number:=extractInt(cmd, 'F: ');
    case number of
      1: spertState.fan:=SpertFanLevel_50;
      2: spertState.fan:=SpertFanLevel_Max;
      3: spertState.fan:=SpertFanLevel_Histeresis;
      4: spertState.fan:=SpertFanLevel_Fixed;
    end;
  end;
end;

procedure TSpert.Send(cmd: String);
begin
  if (comportConnected = true) then
  begin
    if cmd <> 'PA?' then FormDebug.Log('[Spert] < ' + cmd);

    comportMutex.Acquire;
    try
      comport.SendString(Utf8ToAnsi(cmd) + COMPORT_DELIM);
      comport.Flush;
    finally
      comportMutex.Release;
      sleep(200);
    end;
  end;
end;

procedure TSpert.Stop;
begin
  running:=False;
  timerKeepAlive.Enabled:=False;
  autoDiscover.Terminate;
  Disconnect;
  FormDebug.Log('[Spert] stopped');
end;

procedure TSpert.Disconnect;
begin
  if comportConnected then
  begin
    FormDebug.Log('[Spert] disconnecting');

    // alwys unlock TX on exit
    txLockOff;

    comportConnected:=False;

    spertState.Reset;
    if Assigned(fOnState) then fOnState(nil);

    timerRead.Enabled:=False;
    timerRead.Free;

    comport.Free;
  end;

  if (autoDiscoverLockFile <> '') and FileExists(autoDiscoverLockFile) then DeleteFile(autoDiscoverLockFile);
  autoDiscoverLockFile:='';
end;

destructor TSpert.Destroy;
begin
  timerKeepAlive.Enabled:=False;
  FreeAndNil(timerKeepAlive);

  FreeAndNil(comportMutex);
  FreeAndNil(spertState);
  FreeAndNil(autoDiscover);

  inherited;
end;

{ set/get }

procedure TSpert.toggleAtuState;
begin
  if spertState.active then
  begin
    ignorePayload:=TSpertPayloadSuspend.Create;

    if spertState.atuActive then
    begin
      spertState.atuActive:=False;

      ignorePayload.waitFor:='ATU_STATE: 0';
      ignorePayload.ignore:='ATU_STATE: 1';

      Send('ATU_STATE: 0')
    end
    else
      begin
        spertState.atuActive:=True;

        ignorePayload.waitFor:='ATU_STATE: 1';
        ignorePayload.ignore:='ATU_STATE: 0';

        Send('ATU_STATE: 1' + COMPORT_DELIM + 'Port_L ?' + COMPORT_DELIM + 'Port_C ?' + COMPORT_DELIM + 'pin_K ?');
      end;
  end;
end;

procedure TSpert.setPower(pwr: Integer);
var
  payload: String;
begin
    spertState.pwr:=pwr;
    payload:='ALC_PWM: ' + IntToStr(pwr);
    ignorePayload:=TSpertPayloadSuspend.Create;
    ignorePayload.waitFor:=payload;
    ignorePayload.ignore:='ALC_PWM: ';
    Send(payload)
end;

procedure TSpert.tuneAtu;
begin
  if Assigned(fOnEvent) then fOnEvent.call(SpertEventBeforeTuneStart);
  Send('TUNE ON');
end;

function TSpert.isActive:Boolean;
begin
  Result:=(running and comportConnected and spertState.active);
end;

function TSpert.getPwr: Word;
begin
  Result:=spertState.pwr;
end;

function TSpert.getBand: String;
begin
  Result:=spertState.band;
end;

function TSpert.getTemp:Word;
begin
  Result:=spertState.temp;
end;

function TSpert.txActive:Boolean;
begin
  Result:=spertState.tx;
end;

function TSpert.getFpwr: Integer;
begin
  Result:=spertState.fpwr;
end;

function TSpert.getFpwrMax: Integer;
begin
  Result:=spertState.fpwrMax;
end;

function TSpert.getFpwrAvg: Integer;
begin
  Result:=Round(spertState.fpwrSum/spertState.fpwrCnt);
end;

function TSpert.getRpwr: Integer;
begin
  Result:=spertState.rpwr;
end;

function TSpert.getRpwrMax: Integer;
begin
  Result:=spertState.rpwrMax;
end;

function TSpert.getRpwrMaxPercent: Double;
begin
  if spertState.fpwrMax > 0
  then Result:=(spertState.rpwrMax / spertState.fpwrMax) * 100
  else Result:=0;
end;

function TSpert.isAtuPresent: Boolean;
begin
  Result:=spertState.atuPresent;
end;

function TSpert.isAtuActive: Boolean;
begin
  Result:=spertState.atuActive;
end;

function TSpert.getAtu_L: Double;
begin
  Result:=calcAtu_LC(spertState.atuLastL, TSpert.ATU_L);
end;

function TSpert.getAtu_C: Double;
begin
  Result:=calcAtu_LC(spertState.atuLastC, TSpert.ATU_C);
end;

function TSpert.getAtu_K: Byte;
begin
  Result:=spertState.atuLastK;
end;

function TSpert.isAtuTunning: Boolean;
begin
  Result:=spertState.atuTune;
end;

function TSpert.getVersion: String;
begin
  Result:=spertState.version;
end;

function TSpert.getFan: TSpertFanLevel;
begin
   Result:=spertState.fan;
end;

procedure TSpert.resetAtuMem;
begin
  Send('CMEM');
end;

procedure TSpert.setFan(fan: TSpertFanLevel);
begin
  case fan of
    SpertFanLevel_50:         Send('F: 1');
    SpertFanLevel_Max:        Send('F: 2');
    SpertFanLevel_Histeresis: Send('F: 3');
    SpertFanLevel_Fixed:      Send('F: 4');
  end;
end;

procedure TSpert.txLockOn;
begin
  if not spertState.tx and not spertState.atuTune then
  begin
    Send('BL:ON');
    spertState.txLocked:=True;
  end;
end;

procedure TSpert.txLockOff;
begin
  Send('BL:OFF');
  spertState.txLocked:=False;
end;

function TSpert.isTxLocked: Boolean;
begin
  Result:=spertState.txLocked;
end;

procedure TSpert.onEvent(event: IEvent);
begin
  fOnEvent:=event;
end;

procedure TSpert.onState(event: TNotifyEvent);
begin
  fOnState:=event;
end;

{ private helpers }

function TSpert.calcAtu_LC(Input: String; Arr: TSpertLC): Double;
var
  Str: String;
  i: Integer;
begin
  Result:=0;
  Str:= ReplaceStr(PadLeft(Input, 8),' ','0');
  for i := 1 to Length(Arr) do
  begin
    if Str.Substring(i-1, 1) = '1' then Result:=Result + Arr[i];
  end;
end;

function TSpert.extractInt(cmd: String; prefix: String): Integer;
var
  n: Integer;
begin
  Result:=-1;
  if TryStrToInt(ReplaceText(cmd, prefix, ''), n) then
    Result:=n;
end;

end.

