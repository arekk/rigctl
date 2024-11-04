unit UnitRig;

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

  { TRigState - represents current state of rig }

  TRigState = class
    public
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
    public
      constructor Create;
      function getFPwrCur: Byte;
      function getFPwrMax: Byte;
      function getRPwrAvgPercent: Double;
      function getSMeter: String;
    private
      function pwrCalc(input: Byte): Byte;
    private
      meterValuePwr: Byte;
      meterValueSwr: Byte;
      meterValueS: Byte;
      fpwrMax: Byte;
      fpwrSum: Integer;
      fpwrCnt: Integer;
      rpwrSum: Integer;
      rpwrCnt: Integer;
  end;

  { TRigPayloadSuspend - allows to suspend incoming rig commands to disable state flapping, eg. durring power or filter updates }

  TRigPayloadSuspend = class
    private
      wf: String;
      ig: String;
    public
      constructor Create(waitFor: String; ignore: String);
      function match(payload: String): Boolean;
      function ignore(payload: String): Boolean;
  end;

  { TRig - all rig communication procedures }

  TRig = class
    private
      configuration: TConfiguration;
      rigState: TRigState;
      comportMutex: TCriticalSection;
      comport: TBlockSerial;
      comportConnected: Boolean;
      comportLastCommTs: Longword;
      timerKeepAlive: TTimer;
      timerQuery: TTimer;
      timerRead: TTimer;
      ignorePayload: TRigPayloadSuspend;

    public
      constructor Create(Config: TConfiguration);
      destructor Destroy; override;
      procedure Start;
      procedure Stop;
      procedure Send(payload: String);
      procedure SetVfoA_frq(frq: Longword);
      procedure SetSplit(active: Boolean);
      procedure SetPtt(active: Boolean);
      procedure SetMode(mode: String);
      procedure SetBand(band: String);
      procedure SetPwr(pwr: Byte);
      procedure SetdrPortGain(gain: Byte);
      procedure toggleVox;
      procedure toggleDnr;
      procedure toggleDnf;
      property state: TRigState Read rigState;

    public
      const BAND_160 = '00';
      const BAND_80  = '01';
      const BAND_60  = '02';
      const BAND_40  = '03';
      const BAND_30  = '04';
      const BAND_20  = '05';
      const BAND_17  = '06';
      const BAND_15  = '07';
      const BAND_12  = '08';
      const BAND_10  = '09';
      const BAND_6   = '10';

    private
      procedure Connect(port: String);
      procedure Disconnect;
      procedure Read;
      procedure ProcessCmd(payload: String);
      procedure onTimerKeepAlive(Sender: TObject);
      procedure onTimerQuery(Sender: TObject);
      procedure onTimerRead(Sender: TObject);
      function trxModeToStateMode(mode: String): String;
  end;

implementation

{ TRigState }

constructor TRigState.Create;
begin
   active:=False;
   ptt:=False;
   fpwrMax:=0;
   fpwrSum:=0;
   fpwrCnt:=0;
   rpwrSum:=0;
   rpwrCnt:=0;
end;

function TRigState.getFPwrCur: Byte;
begin
   Result:=pwrCalc(meterValuePwr)
end;

function TRigState.getFPwrMax: Byte;
begin
   Result:=pwrCalc(fpwrMax)
end;

function TRigState.pwrCalc(input: Byte): Byte;
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

function TRigState.getRPwrAvgPercent:Double;
begin
  if (fpwrSum > 0) and (rpwrSum > 0)
    then Result:=(((rpwrSum*0.1)/rpwrCnt)/(fpwrSum/fpwrCnt))*100
    else Result:=0;
end;

function TRigState.getSMeter: String;
var
  input: Integer;
begin
  input:=meterValueS;
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

{ TRigPayloadSuspend }

constructor TRigPayloadSuspend.Create(waitFor: String; ignore: String);
begin
  wf:=waitFor;
  ig:=ignore;
  FormDebug.Log('[Rig] suspending payload ' + ig + ' - waiting for ' + wf);
end;

function TRigPayloadSuspend.match(payload: String): Boolean;
begin
  if (payload = wf) then Result:=True else Result:=False;
  if Result then FormDebug.Log('[Rig] suspending payload - release on ' + payload);
end;

function TRigPayloadSuspend.ignore(payload: String): Boolean;
begin
  if StartsStr(ig, payload) then Result:=True else Result:=False;
end;

{ TRig }

constructor TRig.Create(Config: TConfiguration);
begin
  timerKeepAlive:=TTimer.Create(nil);
  timerKeepAlive.Enabled:=False;

  configuration:=Config;

  comportMutex:=TCriticalSection.Create;

  rigState:=TRigState.Create;
  rigState.active:=False;

  comportConnected:=False;
end;

procedure TRig.Start;
begin
  if not timerKeepAlive.Enabled then
  begin
    comportLastCommTs:=DateTimeToUnix(Now());

    timerKeepAlive.Interval:=1000;
    timerKeepAlive.OnTimer:=@onTimerKeepAlive;
    timerKeepAlive.Enabled:=True;
  end;
end;

procedure TRig.onTimerKeepAlive(Sender: TObject);
var
  searchResult: TSearchRec;
  port: String;
begin
  if not comportConnected then
  begin
    port:=Configuration.Settings.trxPort;

    if port='AUTO' then
    begin
      if FindFirst('/dev/tty.SLAB_USBtoUART*', faAnyFile, searchResult) = 0 then
      begin
        repeat
        begin
          { probing ports with sending there communitation doesn't works - probably library issue }
          if searchResult.Name <> 'tty.SLAB_USBtoUART' then
          begin
            port:='/dev/' + searchResult.Name;
            FormDebug.Log('[Rig] discovered port ' + port);
            Break;
          end;
        end
        until FindNext(searchResult) <> 0;
        FindClose(searchResult);
      end;
    end;
    if port <> 'AUTO'
      then Connect(port)
      else FormDebug.Log('[Rig] port not discovered');
  end;

  if comportConnected and rigState.active and ((comportLastCommTs + 5) <  DateTimeToUnix(Now())) then Disconnect;
end;

procedure TRig.Connect(port: String);
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

      FormDebug.Log('[Rig] connetion to ' + port +  ' -> ' + comport.LastErrorDesc + ' code: ' + Inttostr(comport.LastError));

      if comport.LastError = 0 then
      begin
        comportConnected:=True;

        comport.Purge;

        Send('PS1;');
        Sleep(1200);
        Send('PS1;');

        timerQuery:=TTimer.Create(nil);
        timerQuery.Interval:=configuration.Settings.trxPool;
        timerQuery.OnTimer:=@onTimerQuery;
        timerQuery.Enabled:=True;

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

procedure TRig.onTimerQuery(Sender: TObject);
begin
  if (comportConnected = true) then
  begin
    comportMutex.Acquire;
    try
      comport.SendString(Utf8ToAnsi('TX;VS;FA;MD0;FB;MD1;ST;RM1;PC;VX;NR0;BC0;EX010417;'));
      if rigState.ptt then comport.SendString(Utf8ToAnsi('RM5;RM6;'));
      Read;
    finally
      comportMutex.Release;
    end;
  end;
end;

procedure TRig.Send(payload: String);
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

procedure TRig.onTimerRead(Sender: TObject);
begin
   Read;
end;

procedure TRig.Read;
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

procedure TRig.ProcessCmd(payload: String);
var
  command: String;
  meter: Char;
  meterValue: Integer;
  wasPtt: Boolean;
begin
  command:=payload.Substring(0, 2);
  case command of
    'EX': begin
      if (payload.Substring(2, 6) = '010417') then rigState.drPortGain:=StrToInt(payload.Substring(8, 3));
    end;

    'BC': rigState.dnf:=(payload.Substring(3, 1) = '1');

    'NR': rigState.dnr:=(payload.Substring(3, 1) = '1');

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
        '0': rigState.vfoA_mode:=trxModeToStateMode(payload.Substring(3, 1));
        '1': rigState.vfoB_mode:=trxModeToStateMode(payload.Substring(3, 1));
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
      wasPtt:=rigState.ptt;
      rigState.ptt:=(StrToInt(payload.Substring(2, 1)) > 0); // 2 - mic 1 - USB
      if not wasPtt and rigState.ptt then
      begin
        FormDebug.Log('[Rig] PTT ON');
        rigState.fpwrMax:=0;
        rigState.fpwrSum:=0;
        rigState.fpwrCnt:=0;
        rigState.rpwrSum:=0;
        rigState.rpwrCnt:=0;
      end;
      if wasPtt and not rigState.ptt then
      begin
        FormDebug.Log('[Rig] PTT OFF');
      end
    end
  end;
end;

{ VFO-A frequency (there is no _, in cmd it's only for readibility) - 21.700 MHz -> 21_270_000 -> FA_021_270_000 (FA + 6 chars left padded with 0) }
procedure TRig.SetVfoA_frq(frq: Longword);
var
  frequency: String;
begin
  frequency:=ReplaceStr(PadLeft(IntToStr(frq), 9), ' ', '0');
  ignorePayload:=TRigPayloadSuspend.Create('FA' + frequency, 'FA');
  Send('FA' + frequency);
  rigState.vfoA_frq:=frq;
end;

procedure TRig.SetdrPortGain(gain: Byte);
var
  payload: String;
begin
  rigState.drPortGain:=gain;
  payload:='EX010417' + ReplaceStr(PadLeft(IntToStr(gain), 3), ' ', '0');
  ignorePayload:=TRigPayloadSuspend.Create(payload, 'EX010417');
  Send(payload);
end;

procedure TRig.SetMode(mode: String);
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
  if translatedMode <> '' then Send('MD' + IntToStr(rigState.vfo) + translatedMode);
end;

procedure TRig.SetSplit(active: Boolean);
begin
  if active then Send('ST1') else Send('ST0');
end;

procedure TRig.SetPtt(active: Boolean);
begin
  if active then Send('TX1') else Send('TX0');
end;

procedure TRig.SetBand(band: String);
begin
  Send('BS' + band);
end;

procedure TRig.SetPwr(pwr: Byte);
var
  payload: String;
begin
  rigState.pwr:=pwr;
  payload:='PC' + ReplaceStr(PadLeft(IntToStr(pwr), 3), ' ', '0');
  ignorePayload:=TRigPayloadSuspend.Create(payload, 'PC');
  Send(payload);
end;

procedure TRig.toggleVox;
begin
  if rigState.vox then begin
    ignorePayload:=TRigPayloadSuspend.Create('VX0', 'VX1');
    Send('VX0')
  end else begin
    ignorePayload:=TRigPayloadSuspend.Create('VX1', 'VX0');
    Send('VX1')
  end;
  rigState.vox:=(not rigState.vox);
end;

procedure TRig.toggleDnr;
begin
    if rigState.dnr then begin
    ignorePayload:=TRigPayloadSuspend.Create('NR00', 'NR01');
    Send('NR00')
  end else begin
    ignorePayload:=TRigPayloadSuspend.Create('NR01', 'NR00');
    Send('NR01')
  end;
  rigState.dnr:=(not rigState.dnr);
end;

procedure TRig.toggleDnf;
begin
  if rigState.dnf then begin
    ignorePayload:=TRigPayloadSuspend.Create('BC00', 'BC01');
    Send('BC00')
  end else begin
    ignorePayload:=TRigPayloadSuspend.Create('BC01', 'BC00');
    Send('BC01')
  end;
  rigState.dnf:=(not rigState.dnf);

  if rigState.dnf then Send(';') else Send(';');
end;

procedure TRig.Stop;
begin
  if rigState.ptt then SetPtt(False);
  timerKeepAlive.Enabled:=False;
  Disconnect;
  FormDebug.Log('[Rig] stopped');
end;

procedure TRig.Disconnect;
begin
  if comportConnected then
    begin
      FormDebug.Log('[Rig] disconnecting');

      timerQuery.Enabled:=False;
      FreeAndNil(timerQuery);

      timerRead.Enabled:=False;
      FreeAndNil(timerRead);

      comportConnected:=False;
      rigState.active:=False;

      comport.CloseSocket;
    end;

  FreeAndNil(comport);
end;

function TRig.trxModeToStateMode(mode: String): String;
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

destructor TRig.Destroy;
begin
  timerKeepAlive.Enabled:=False;
  FreeAndNil(timerKeepAlive);

  FreeAndNil(comportMutex);
  FreeAndNil(rigState);

  inherited;
end;

end.

