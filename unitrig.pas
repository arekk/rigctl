unit UnitRig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  LazSynaser,
  Cthreads,
  UnitFormDebug;

type
  TRigPortDiscoverThread = class(TThread)
    public
      constructor Create(port: String; portRate: Integer; send: String; expect: String);
      function getPort:String;

    protected
      procedure Execute; override;

    private
      fResult: Boolean;
      fStatusText : string;
      fPort: String;
      fPortRate: Integer;
      fSend: String;
      fExpect: String;
      procedure ShowStatus;
  end;

  TRigEvents = class
    public
      const BAND_CHANGE = 1;
      const MODE_CHANGE = 2;
  end;

  TRigEvent = interface
    procedure call(event: Byte);
  end;

  TRig = interface
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
    function getVfo:Byte;
    function getMode:String;
    function getFrq:Longword;
    function getBand:Byte;
    function getdrPortGain: Byte;
    function getFPwrCur: Byte;
    function getFPwrMax: Byte;
    function getRPwrAvgPercent: Double;
    function getSMeter: String;
    function getVersion: String;
  end;

implementation

{ TPortDiscoverThread }

constructor TRigPortDiscoverThread.Create(port: String; portRate: Integer; send: String; expect: String);
begin
  inherited Create(False);
  FreeOnTerminate:=False;
  fPort:=port;
  fPortRate:=portRate;
  fSend:=send;
  fExpect:=expect;
  fResult:=False;
end;

procedure TRigPortDiscoverThread.ShowStatus;
begin
  FormDebug.Log('[Rig] PortDiscover - ' + fStatusText);
end;

procedure TRigPortDiscoverThread.Execute;
var
  comport: TBlockSerial;
  payload: String;
begin
  comport:=TBlockSerial.Create;
  comport.LinuxLock:=False;
  comport.NonBlock:=True;
  comport.Connect(fPort);
  comport.Config(fPortRate, 8, 'N', SB1, False, True);
  fStatusText:='connection to ' + fPort +  ' -> ' + comport.LastErrorDesc + ' code: ' + Inttostr(comport.LastError);
  Synchronize(@Showstatus);
  if comport.LastError = 0 then
  begin
    comport.Purge;
    comport.SendString(Utf8ToAnsi(fSend));
    Sleep(1000);
    while comport.WaitingData > 0 do begin
      payload:=Trim(AnsiToUtf8(comport.RecvTerminated(10, ';')));
      if ContainsText(payload, fExpect) then fResult:=True;
    end;
  end;
  comport.Purge;
  comport.CloseSocket;
  FreeAndNil(comport);
  Terminate;
end;

function TRigPortDiscoverThread.getPort:String;
begin
  if fResult then Result:=fPort else Result:='';
end;

end.
