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
  UnitSettings,
  UnitFormDebug;

type
  TSpertEvent = interface
    procedure call(event: Byte);
  end;

  TSpertState = class
    public
    const EVENT_BEFORE_TUNE_START = 1;
    const EVENT_AFTER_TUNE_END = 2;
    public
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
    version: String;
    onEvent: TSpertEvent;
  public
    constructor Create;
  private
    fpwrSum: Integer;
    fpwrCnt: Integer;
    rpwrSum: Integer;
    rpwrCnt: Integer;
  end;

  TSpertPayloadSuspend = class
    waitFor: String;
    ignore: String;
  end;

  TSpert = class
    private
      configuration: TConfiguration;
      spertState: TSpertState;
      comportMutex: TCriticalSection;
      comport: TBlockSerial;
      comportConnected: Boolean;
      comportLastCommTs: Integer;
      timerKeepAlive: TTimer;
      timerRead: TTimer;
      ignorePayload: TSpertPayloadSuspend;
    public
      constructor Create(Config: TConfiguration);
      destructor Destroy; override;
      procedure Start;
      procedure toggleAtuState;
      procedure setPower(pwr: Integer);
      procedure tuneAtu;
      procedure Stop;
      property state: TSpertState Read spertState;
    private
      procedure onTimerKeepAlive(Sender: TObject);
      procedure Connect(port: String);
      procedure onTimerRead(Sender: TObject);
      procedure Read;
      procedure Send(cmd: String);
      procedure ProcessCmd(cmd: String);
      function extractInt(cmd: String; prefix: String): Integer;
      procedure Disconnect;
    end;

implementation

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
end;

constructor TSpert.Create(Config: TConfiguration);
begin
  timerKeepAlive:=TTimer.Create(nil);
  timerKeepAlive.Enabled:=False;

  Configuration:=Config;

  comportMutex:=TCriticalSection.Create;

  spertState:=TSpertState.Create;
  spertState.active:=False;

  comportConnected:=False;
end;

procedure TSpert.Start;
begin
  if not timerKeepAlive.Enabled then
  begin
    timerKeepAlive:=TTimer.Create(nil);
    timerKeepAlive.Interval:=1000;
    timerKeepAlive.OnTimer:=@onTimerKeepAlive;
    timerKeepAlive.Enabled:=True;
  end;
end;

procedure TSpert.onTimerKeepAlive(Sender: TObject);
var
  searchResult: TSearchRec;
  port: String;
begin
  if not comportConnected then begin
    port:=Configuration.Settings.spertPort;

    if port='AUTO' then
    begin
      if FindFirst('/dev/tty.usbserial-*', faAnyFile, searchResult) = 0 then
      begin
        repeat begin
          port:='/dev/' + searchResult.Name;
          FormDebug.Log('[Spert] discovered port ' + port);
          Break;
        end until FindNext(searchResult) <> 0;
        FindClose(searchResult);
      end;
    end;
    if port <> 'AUTO'
      then Connect(port)
      else  FormDebug.Log('[Spert] port not discovered');
  end;

  // ping every 1s to check is connection still alive
  if comportConnected then Send('PA?');

  // checking timestamp of last meessage read from SPert
  if comportConnected and spertState.active and ((comportLastCommTs + 5) <  DateTimeToUnix(Now()))
    then Disconnect
end;

procedure TSpert.Connect(port: String);
begin
  if  (port <> '') and not comportConnected then
  begin
    comportMutex.Acquire;
    try
      FormDebug.Log('[Spert] connecting to ' + Configuration.Settings.spertPort + ' at ' + IntToStr(Configuration.Settings.spertPortRate));

      comport:=TBlockSerial.Create;
      comport.LinuxLock:=False;
      comport.NonBlock:=True;
      comport.Connect(port);
      comport.Config(Configuration.Settings.spertPortRate, 8, 'N', SB1, False, True);

      FormDebug.Log('[Spert] connetion to ' + port +  ' -> ' + comport.LastErrorDesc + ' code: ' + Inttostr(comport.LastError));

      if comport.LastError = 0 then
      begin
        comportConnected:=true;

        comport.Purge;

        { main timer handling periodic read from SPert serial port }
        timerRead:=TTimer.Create(nil);
        timerRead.Interval:=Configuration.Settings.spertPool;
        timerRead.OnTimer:=@onTimerRead;
        timerRead.Enabled:=True;

        Send('???');
        Read;
        Send('ATU?');
        Read;

        if (Configuration.Settings.spertStartupFan > 0) and (Configuration.Settings.spertStartupFan <= 4) then Send('F: ' + IntToStr(Configuration.Settings.spertStartupFan));

        if spertState.pwr > 0 then
        begin
          Send('ALC_PWM: 0');
          Send('ALC_PWM: ' + IntToStr(spertState.pwr));
        end;

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
          r:=comport.RecvTerminated(10, AnsiString(#13) + AnsiString(#10));
          payload:=Trim(AnsiToUtf8(r));
          if payload <> '' then begin
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
  txWas: Boolean;
  atuTuneWas: Boolean;
begin
  txWas:=spertState.tx;
  atuTuneWas:=spertState.atuTune;

  if cmd <> 'PA_SP7SP' then FormDebug.Log('[Spert] < "' + cmd + '"');

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

  // PTT on/off
  if StartsStr('TX:', cmd) then
  begin
    value:=ReplaceText(cmd, 'TX: ', '');
    if value='ON' then
    begin
      spertState.tx:=True;
      spertState.fpwrMax:=0;
      spertState.fpwrSum:=0;
      spertState.fpwrCnt:=0;
      spertState.rpwrSum:=0;
      spertState.rpwrCnt:=0;
    end;
    if value='OFF' then
    begin
      spertState.tx:=False;
    end;
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

  // ATU tuning started event
  if cmd='TUNE' then
  begin
    spertState.atuTune:=True;
    spertState.tx:=True;
  end;

  // ATU tuning finished event
  if cmd='TUNE OFF' then
  begin
    spertState.atuTune:=False;
    spertState.tx:=False;
    if Assigned(spertState.onEvent) and atuTuneWas then spertState.onEvent.call(TSpertState.EVENT_AFTER_TUNE_END);
  end;

  // ATU colors
  if (cmd='GREY') or (cmd='RED') or (cmd='GREEN') then spertState.atuColor:=cmd;

  // firmware version
  if StartsStr('Ver:', cmd) then spertState.version:='SPert 1000 (' + ReplaceText(cmd, 'Ver: ', '') + ')';
end;

function TSpert.extractInt(cmd: String; prefix: String): Integer;
var
  n: Integer;
begin
  Result:=-1;
  if TryStrToInt(ReplaceText(cmd, prefix, ''), n) then
    Result:=n;
end;

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
  if Assigned(spertState.onEvent) then spertState.onEvent.call(TSpertState.EVENT_BEFORE_TUNE_START);
  Send('TUNE ON');
end;

procedure TSpert.Send(cmd: String);
begin
  if (comportConnected = true) then
  begin
    if cmd <> 'PA?' then FormDebug.Log('[Spert] > ' + cmd);
    comportMutex.Acquire;
    try
      comport.SendString(Utf8ToAnsi(cmd) + AnsiString(#13) + AnsiString(#10));
    finally
      comportMutex.Release;
      sleep(200);
    end;
  end;
end;

procedure TSpert.Stop;
begin
  timerKeepAlive.Enabled:=False;
  Disconnect;
  FormDebug.Log('[Spert] stopped');
end;

procedure TSpert.Disconnect;
begin
  if comportConnected then
  begin
    FormDebug.Log('[Spert] disconnecting');

    // if disconnectinb because error, eg. serial port interference run following commands to set proper state before disconnecting
    ProcessCmd('TUNE OFF');
    ProcessCmd('TX: OFF');

    spertState.active:=False;

    timerRead.Enabled:=False;
    FreeAndNil(timerRead);

    comport.CloseSocket;

    comportConnected:=false;
  end;
end;

destructor TSpert.Destroy;
begin
  timerKeepAlive.Enabled:=False;
  FreeAndNil(timerKeepAlive);
  FreeAndNil(comportMutex);
  FreeAndNil(spertState);

  inherited;
end;

end.

