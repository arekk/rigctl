unit UnitFormSpert;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes,
  SysUtils,
  Forms,
  CocoaAll,
  Controls,
  Dialogs,
  Menus,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  Buttons,
  Graphics,
  LCLType,
  UnitSettings,
  UnitSpert,
  UnitRig,
  UnitFormRig,
  UnitFormDebug;

type
  { TRigSpertEvent - allows for Spert->Rig integration and other macros - will fire call when something will change }

  TRigSpertEvent = class(TInterfacedObject, TSpertEvent)
    private
      Rig: TRig;
      trxTunePower: Byte;
      trxPowerSavePoint: Byte;
    public
      constructor Create(r: TRig; tp: Byte);
      procedure call(event: Byte);
  end;

  { TFormSpert }

  TFormSpert = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    TrackBar1: TTrackBar;

    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1MouseEnter(Sender: TObject);
    procedure TrackBar1MouseLeave(Sender: TObject);
    procedure ReloadConfiguration;

    private
      Configuration: TConfiguration;
      Spert: TSpert;
  end;

var
  FormSpert: TFormSpert;

implementation

{$R *.lfm}

{ TRigSpertEvent }

constructor TRigSpertEvent.Create(r: TRig; tp: Byte);
begin
  Rig:=r;
  trxTunePower:=tp;
  trxPowerSavePoint:=0;
end;

procedure TRigSpertEvent.call(event: Byte);
begin
  FormDebug.Log('[Spert] event: ' + IntToStr(event));

  case event of
    TSpertState.EVENT_BEFORE_TUNE_START: begin
      { ATU tunning can set TRX power to 5W }
      if (trxTunePower > 0) and (trxPowerSavePoint = 0) and Assigned(Rig) and Rig.state.active and (not Rig.state.ptt) then
      begin
        FormDebug.Log('[Spert] setting trx power to: ' + IntToStr(trxTunePower));
        trxPowerSavePoint:=Rig.state.pwr;
        Rig.SetPwr(trxTunePower);
      end;
    end;

    TSpertState.EVENT_AFTER_TUNE_END: begin
      { restore trx power if atu stopped tuning }
      if (trxPowerSavePoint > 0) and Assigned(Rig) and Rig.state.active then
      begin
        FormDebug.Log('[Spert] restoring trx power: ' + IntToStr(trxPowerSavePoint));
        Rig.SetPwr(trxPowerSavePoint);
      end;
      trxPowerSavePoint:=0;
    end;
  end;
end;

{ TFormSpert }

procedure TFormSpert.FormCreate(Sender: TObject);
begin
  Configuration:=TConfiguration.Create(Application.Location);
  Configuration.Load;

  Visible:=Configuration.Settings.spertEnabled;

  Spert:=TSpert.Create(Configuration);
  Spert.state.onEvent:=TRigSpertEvent.Create(FormRig.Rig, Configuration.Settings.TrxTunePower);
end;

procedure TFormSpert.FormShow(Sender: TObject);
begin
  NSView(Handle).window.setFrameAutosaveName(NSSTR(ClassName));

  GroupBox1.Enabled:=False;
  GroupBox2.Enabled:=False;
  StatusBar1.Panels.Items[1].Text:= 'Not connected';

  Spert.Start;

  Timer1.Interval:=Configuration.Settings.spertPool;
  Timer1.Enabled:=True;
end;

procedure TFormSpert.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   Spert.Stop;
   CloseAction:=caHide;
end;

procedure TFormSpert.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Spert);
  FreeAndNil(Configuration);
end;

{ global keyboard shortcuits}
procedure TFormSpert.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Spert.state.active and not Spert.state.tx and Button6.Enabled and (Key = VK_A) then Button6Click(Sender);
  if Spert.state.active and not Spert.state.tx and Button4.Enabled and (Key = VK_T) then Button4Click(Sender);
  if Spert.state.active and not Spert.state.tx and TrackBar1.Enabled then
  begin
    case Key of
      VK_1: TrackBar1.Position:=10;
      VK_2: TrackBar1.Position:=20;
      VK_3: TrackBar1.Position:=30;
      VK_4: TrackBar1.Position:=40;
      VK_5: TrackBar1.Position:=50;
      VK_6: TrackBar1.Position:=60;
      VK_7: TrackBar1.Position:=70;
      VK_8: TrackBar1.Position:=80;
      VK_9: TrackBar1.Position:=90;
      VK_0: TrackBar1.Position:=100;
    end;
  end;
end;

{ power: fixed 100W }
procedure TFormSpert.Button1Click(Sender: TObject);
begin
  if Spert.state.active and not Spert.state.tx and TrackBar1.Enabled
    then TrackBar1.Position:=10;
end;

{ power: fixed 300W }
procedure TFormSpert.Button5Click(Sender: TObject);
begin
  if Spert.state.active and not Spert.state.tx and TrackBar1.Enabled
    then TrackBar1.Position:=30;
end;

{ power: fixed 500W }
procedure TFormSpert.Button2Click(Sender: TObject);
begin
  if Spert.state.active and not Spert.state.tx and TrackBar1.Enabled
    then TrackBar1.Position:=50;
end;

{ power: fixed 700W }
procedure TFormSpert.Button3Click(Sender: TObject);
begin
  if Spert.state.active and not Spert.state.tx and TrackBar1.Enabled
    then TrackBar1.Position:=70;
end;

{ ATU on/by-pass }
procedure TFormSpert.Button6Click(Sender: TObject);
begin
  if Spert.state.active and not Spert.state.tx and Button6.Enabled then SPert.toggleAtuState;
end;

{ ATU tune }
procedure TFormSpert.Button4Click(Sender: TObject);
begin
  if Spert.state.active and not Spert.state.tx and Button4.Enabled then
  begin
    Spert.tuneAtu;
    Button4.Enabled:=False;
  end;
end;

procedure TFormSpert.TrackBar1MouseEnter(Sender: TObject);
begin
  TrackBar1.Tag:=1;
end;

procedure TFormSpert.TrackBar1MouseLeave(Sender: TObject);
begin
  TrackBar1.Tag:=0;
end;

procedure TFormSpert.TrackBar1Change(Sender: TObject);
begin
  if Spert.state.active and not Spert.state.tx and TrackBar1.Enabled and (TrackBar1.Position >= 0) and (TrackBar1.Position <= 100) then Spert.setPower(TrackBar1.Position);
end;

procedure TFormSpert.Timer1Timer(Sender: TObject);
begin
  BeginFormUpdate;

  if Spert.state.active then
  begin
    GroupBox1.Enabled:=True;

    // power
    if (TrackBar1.Tag = 0) and (Spert.state.pwr <> TrackBar1.Position) then
    begin
      TrackBar1.Enabled:=false;
      TrackBar1.Position:=Spert.state.pwr;
      TrackBar1.Enabled:=true;
    end;

    Label1.Caption:='Power ' + IntToStr(Spert.state.pwr * 10) + ' W';

    // band
    if Spert.state.band <> StatusBar1.Panels.Items[0].Text then StatusBar1.Panels.Items[0].Text:=Spert.state.band;

    // temp
    Panel2.Caption:=IntToStr(Spert.state.temp);

    if (Spert.state.temp > 0) AND (Spert.state.temp < 40) then
    begin
      Panel2.Color:=clGreen;
      Panel2.Font.Color:=clWhite;
    end;
    if (Spert.state.temp >= 40) AND (Spert.state.temp < 45) then
    begin
      Panel2.Color:=clYellow;
      Panel2.Font.Color:=clBlack;
    end;
    if (Spert.state.temp >= 45) then
    begin
      Panel2.Color:=clRed;
      Panel2.Font.Color:=clWhite;
    end;

    // tx
    if Spert.state.tx then
    begin
      Panel3.Color:=clGreen;
      Panel3.Caption:=IntToStr(Spert.state.fpwrAvg);

      Panel6.Color:=clGreen;
      Panel6.Caption:=IntToStr(Spert.state.fpwrMax);

      Panel7.Color:=clGreen;
      Panel7.Caption:=IntToStr(Spert.state.fpwrCur);

      Panel4.Caption:=Format('%.1f', [Spert.state.rpwrAvgPercent]);

      if Spert.state.rpwrAvgPercent < 1 then
      begin
        Panel4.Color:=clGreen;
        Panel4.Font.Color:=clWhite;
      end;
      if (Spert.state.rpwrAvgPercent >= 1) and (Spert.state.rpwrAvgPercent < 5) then
      begin
        Panel4.Color:=clYellow;
        Panel4.Font.Color:=clBlack;
      end;
      if (Spert.state.rpwrAvgPercent >= 5) then
      begin
        Panel4.Color:=clRed;
        Panel4.Font.Color:=clWhite;
      end;

      GroupBox1.Enabled:=False;
      GroupBox2.Enabled:=False;
    end;

    if not Spert.state.tx then
    begin
      Panel3.Color:=clGray;
      Panel6.Color:=clGray;
      Panel7.Color:=clGray;

      if Panel4.Color=clGreen then Panel4.Color:=clGray; // set back to gray if there was no excessive reflected power

      GroupBox1.Enabled:=True;
      GroupBox2.Enabled:=True;
    end;

    { ATU }

    // atu presence
    GroupBox2.Enabled:=SPert.state.atuPresent;

    // atu is starting to tune
    if Spert.state.atuTune then
    begin
      Button6.Enabled:=False;
      Panel1.Color:=clMaroon;
      Button4.Enabled:=False;
    end;

    // atu is not tunning - update state
    if not Spert.state.atuTune then begin

      // atu is on
      if Spert.state.atuActive then
      begin
        Panel5.Color:=clGreen;

        if Spert.state.atuColor = 'GREY' then Panel1.Color:=clGray;
        if Spert.state.atuColor = 'RED' then Panel1.Color:=clRed;
        if Spert.state.atuColor = 'GREEN' then Panel1.Color:=clGreen;

        Button6.Enabled:=True;
        Button4.Enabled:=True;
      end;

      // atu is in by-pass state (off)
      if not Spert.state.atuActive then
      begin
        Panel5.Color:=clGray;
        Panel1.Color:=clGray;
        Button4.Enabled:=False;
      end;
    end;

    // version
    StatusBar1.Panels.Items[1].Text:=Spert.state.version;
  end;

  if not Spert.state.active then
  begin
    GroupBox1.Enabled:=False;
    GroupBox2.Enabled:=False;
    StatusBar1.Panels.Items[1].Text:= 'Not connected';

    Panel1.Color:=clGray;

    Panel2.Color:=clGray; // temp
    Panel2.Caption:='0';

    Panel3.Color:=clGray; // P-fwd(avg)
    Panel3.Caption:='0';

    Panel4.Color:=clGray; // P-ref
    Panel4.Caption:='0.0';

    Panel5.Color:=clGray;

    Panel6.Color:=clGray; // P-fwd(max)
    Panel6.Caption:='0';

    Panel7.Color:=clGray; // P-fwd(c)
    Panel7.Caption:='0';
  end;

  EndFormUpdate;
end;

procedure TFormSpert.ReloadConfiguration;
begin
  Configuration.Load;
  Spert.Stop;
  Spert.Start;
end;

end.
