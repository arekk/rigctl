unit UnitSpert;

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
  UnitSerialPortAutoDiscover,
  UnitSettings,
  UnitFormDebug;

type
  TSpertEventType = (SpertEventBeforeTuneStart, SpertEventAtferTuneStop, SpertEventTxStart, SpertEventTxStop);

  TSpertEvent = interface
    procedure call(event: TSpertEventType);
  end;

  TSpertFanLevel = (SpertFanLevel_Undef, SpertFanLevel_50, SpertFanLevel_Max, SpertFanLevel_Histeresis, SpertFanLevel_Fixed);

  TSpertState = class
    public
      constructor Create;
    private
      active: Boolean;
      pwr: Word;
      band: String;
      temp:Word;
      tx: Boolean;
      fpwrMax: Integer;
      fpwrAvg: Integer;
      fpwrCur: Integer;
      rpwrAvgPercent: Double;
      atuPresent: Boolean;
      atuActive: Boolean;
      atuColor: String;
      atuTune: Boolean;
      fan: TSpertFanLevel;
      version: String;
      fpwrSum: Integer;
      fpwrCnt: Integer;
      rpwrSum: Integer;
      rpwrCnt: Integer;
      txLocked: Boolean;
  end;

  TSpertPayloadSuspend = class
    waitFor: String;
    ignore: String;
  end;

  TSpert = class
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
      comportLastCommTs: Integer;
      timerRead: TTimer;
      ignorePayload: TSpertPayloadSuspend;
      onEvent: TSpertEvent;
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
      procedure Disconnect;
    public
      const COMPORT_DELIM = AnsiString(#13) + AnsiString(#10);
    public
      constructor Create(Config: TConfiguration);
      destructor Destroy; override;
      procedure setEventHandler(handler: TSpertEvent);
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
      function getFpwrMax: Integer;
      function getFpwrAvg: Integer;
      function getFpwrCur: Integer;
      function getRpwrAvgPercent: Double;
      function isTxLocked: Boolean;
      function isAtuPresent: Boolean;
      function isAtuActive: Boolean;
      function getAtuColor: String;
      function isAtuTunning: Boolean;
      function getVersion: String;
      function getFan: TSpertFanLevel;
      procedure setFan(fan: TSpertFanLevel);
      procedure resetAtuMem;
  end;

implementation


{ TSpertState }

constructor TSpertState.Create;
begin
  active:=False;
  pwr:=0;
  band:='';
  temp:=0;
  tx:=False;
  fpwrMax:=0;
  fpwrAvg:=0;
  fpwrCur:=0;
  rpwrAvgPercent:=0;
  atuPresent:=False;
  atuActive:=False;
  atuColor:='GREY';
  atuTune:=False;
  version:='';
  fan:=SpertFanLevel_Undef;
  txLocked:=False;
end;

{ TSpert }

constructor TSpert.Create(Config: TConfiguration);
begin
  configuration:=Config;

  autoDiscover:=TSerialPortDiscover.Create(configuration.getConfigDirectory);
  autoDiscoverLockFile:='';

  spertState:=TSpertState.Create;
  spertState.active:=False;

  comportMutex:=TCriticalSection.Create;

  timerKeepAlive:=TTimer.Create(nil);
  timerKeepAlive.Enabled:=False;

  comportConnected:=False;
end;

procedure TSpert.setEventHandler(handler: TSpertEvent);
begin
  onEvent:=handler;
end;

procedure TSpert.Start;
begin
  running:=True;

  timerKeepAlive:=TTimer.Create(nil);
  timerKeepAlive.Interval:=1000;
  timerKeepAlive.OnTimer:=@onTimerKeepAlive;
  timerKeepAlive.Enabled:=True;
end;

procedure TSpert.onTimerKeepAlive(Sender: TObject);
begin
  FormDebug.Log('[Spert] connection: ' + BoolToStr(comportConnected, True));

  if not comportConnected then begin
    if configuration.Settings.spertPort = 'AUTO' then
    begin
      autoDiscover.OnSuccess:=@onPortDiscover;
      autoDiscover.OnFinish:=@onPortDiscoverFinish;
      if autoDiscover.Discover(TSerialPortDiscoverParams.Create('SPert', Configuration.Settings.spertPortRate, COMPORT_DELIM, Utf8ToAnsi('PA?'), Utf8ToAnsi('PA_SP7SP'))) > 0
        then timerKeepAlive.Enabled:=False;
    end
    else
    begin
      autoDiscoverLockFile:=autoDiscover.LockPort(configuration.Settings.spertPort);
      Connect(configuration.Settings.spertPort);
    end;
  end;

  // ping every 1s to check is connection still alive
  // we don't need to ask about current status of amplifier because device will send any update made physically
  if comportConnected
    then Send('PA?');

  // checking timestamp of last meessage read from SPert
  if comportConnected and spertState.active and ((comportLastCommTs + 5) <  DateTimeToUnix(Now()))
    then Disconnect
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

      FormDebug.Log('[Spert] connetion to ' + port +  ' -> ' + comport.LastErrorDesc + ' code: ' + Inttostr(comport.LastError));

      if comport.LastError = 0 then
      begin
        comportConnected:=true;

        { main timer handling periodic read from SPert serial port }
        timerRead:=TTimer.Create(nil);
        timerRead.Interval:=Configuration.Settings.spertPool;
        timerRead.OnTimer:=@onTimerRead;
        timerRead.Enabled:=True;

        // set fan if required
        if (Configuration.Settings.spertStartupFan > 0) and (Configuration.Settings.spertStartupFan <= 4)
          then comport.SendString(Utf8ToAnsi('F: ' + IntToStr(Configuration.Settings.spertStartupFan)) + COMPORT_DELIM);

        // just after connect query current state
        comport.SendString(Utf8ToAnsi('???') + COMPORT_DELIM);
        comport.SendString(Utf8ToAnsi('ATU?') + COMPORT_DELIM);

      end;
    finally
       comportMutex.Release;
    end;
  end;
end;

procedure TSpert.onTimerRead(Sender: TObject);
begin
  Read;
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
            FormDebug.Log('[Spert] > ' + payload);
            spertState.active:=True;
            comportLastCommTs:=DateTimeToUnix(Now());

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
  atuTuneWas: Boolean;
  txWas: Boolean;
begin
  atuTuneWas:=spertState.atuTune;
  txWas:=spertState.tx;

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

  // outout power - showing average
  if StartsStr('O:', cmd) then
  begin
    number:= extractInt(cmd, 'O: ');
    if (number <= 1200) then
    begin
      if (number > spertState.fpwrMax) then spertState.fpwrMax:=number;

      spertState.fpwrSum:=spertState.fpwrSum+number;
      spertState.fpwrCnt:=spertState.fpwrCnt+1;
      spertState.fpwrAvg:=Round(spertState.fpwrSum/spertState.fpwrCnt);

      spertState.fpwrCur:=number;
    end;
  end;

  // reflected power - showing average
  if StartsStr('R:', cmd) then
  begin
    number:= extractInt(cmd, 'R: ');
    if (number <= 120) then
    begin
      spertState.rpwrSum:=spertState.rpwrSum+number;
      spertState.rpwrCnt:=spertState.rpwrCnt+1;

      if (spertState.fpwrSum > 0) and (spertState.rpwrSum > 0)
        then spertState.rpwrAvgPercent:=((spertState.rpwrSum/spertState.rpwrCnt)/(spertState.fpwrSum/spertState.fpwrCnt))*100
        else spertState.rpwrAvgPercent:=0;
    end;
  end;

  {
  if StartsStr('BL:', cmd) then
  begin
    value:=ReplaceText(cmd, 'BL: ', '');
    spertState.txLocked:=(value = 'ON');
  end;
  }

  // PTT on/off
  if StartsStr('TX:', cmd) then
  begin
    value:=ReplaceText(cmd, 'TX: ', '');
    if value='ON' then
    begin
      spertState.tx:=True;
      if Assigned(onEvent) and txWas then onEvent.call(SpertEventTxStart);
      spertState.fpwrMax:=0;
      spertState.fpwrSum:=0;
      spertState.fpwrCnt:=0;
      spertState.rpwrSum:=0;
      spertState.rpwrCnt:=0;
    end;
    if value='OFF' then
    begin
      spertState.tx:=False;
      if Assigned(onEvent) and txWas then onEvent.call(SpertEventTxStop);
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
    if Assigned(onEvent) and atuTuneWas then onEvent.call(SpertEventAtferTuneStop);
  end;

  // ATU colors
  if (cmd='GREY') or (cmd='RED') or (cmd='GREEN') then spertState.atuColor:=cmd;

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
    FormDebug.Log('[Spert] < ' + cmd);
    comportMutex.Acquire;
    try
      comport.SendString(Utf8ToAnsi(cmd) + COMPORT_DELIM);
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

    // if disconnecting because error, eg. serial port interference run following commands to set proper state before disconnecting
    ProcessCmd('TUNE OFF');
    ProcessCmd('TX: OFF');

    spertState.active:=False;

    timerRead.Enabled:=False;
    FreeAndNil(timerRead);

    comport.CloseSocket;

    comportConnected:=false;

    if (autoDiscoverLockFile <> '') and FileExists(autoDiscoverLockFile) then DeleteFile(autoDiscoverLockFile);
    autoDiscoverLockFile:='';
  end;
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

{ set - get }

procedure TSpert.toggleAtuState;
begin
  if spertState.active then
  begin
    if spertState.atuActive
      then Send('ATU_STATE: 0')
      else Send('ATU_STATE: 1');
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
  if Assigned(onEvent) then onEvent.call(SpertEventBeforeTuneStart);
  Send('TUNE ON');
end;

function TSpert.isActive:Boolean;
begin
  Result:=spertState.active;
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

function TSpert.getFpwrMax: Integer;
begin
  Result:=spertState.fpwrMax;
end;

function TSpert.getFpwrAvg: Integer;
begin
  Result:=spertState.fpwrAvg;
end;

function TSpert.getFpwrCur: Integer;
begin
  Result:=spertState.fpwrCur;
end;

function TSpert.getRpwrAvgPercent: Double;
begin
  Result:=spertState.rpwrAvgPercent;
end;

function TSpert.isAtuPresent: Boolean;
begin
  Result:=spertState.atuPresent;
end;

function TSpert.isAtuActive: Boolean;
begin
  Result:=spertState.atuActive;
end;

function TSpert.getAtuColor: String;
begin
  Result:=spertState.atuColor;
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

{ helpers }

function TSpert.extractInt(cmd: String; prefix: String): Integer;
var
  n: Integer;
begin
  Result:=-1;
  if TryStrToInt(ReplaceText(cmd, prefix, ''), n) then
    Result:=n;
end;

end.

