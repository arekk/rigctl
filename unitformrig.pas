unit UnitFormRig;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes,
  SysUtils,
  Forms,
  CocoaAll,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  LCLType,
  LCLIntf,
  ComCtrls,
  Buttons,
  StrUtils,
  UnitSettings,
  UnitFlrigServer,
  UnitRig,
  UnitRigDummy,
  UnitRigFTdx10,
  UnitRigFT991A,
  UnitFormDebug;

type
  { TFormRig }
  TFormRig = class(TForm)
    BitBtnSplit: TBitBtn;
    BitBtnSplitPlus: TBitBtn;
    BitBtnSplitMinus: TBitBtn;
    BitBtnTXW: TBitBtn;
    BitBtnDNF: TBitBtn;
    BitBtnVOX: TBitBtn;
    BitBtnDNR: TBitBtn;
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    LabelPwr: TLabel;
    LabelDgain: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PanelPwr: TPanel;
    PanelSmetr: TPanel;
    PanelSwr: TPanel;
    PanelVfoFrq: TPanel;
    PanelVfoMode: TPanel;
    PanelVfoAB: TPanel;
    StatusBar1: TStatusBar;
    TrackBarPwr: TTrackBar;
    TrackBarDgain: TTrackBar;
    procedure BitBtnSplitClick(Sender: TObject);
    procedure BitBtnSplitMinusClick(Sender: TObject);
    procedure BitBtnSplitPlusClick(Sender: TObject);
    procedure BitBtnTXWClick(Sender: TObject);
    procedure BitBtnDNFClick(Sender: TObject);
    procedure BitBtnDNRClick(Sender: TObject);
    procedure BitBtnVOXClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TrackBarPwrChange(Sender: TObject);
    procedure TrackBarDgainChange(Sender: TObject);
  private
    Configuration: TConfiguration;
    flRigServer: TFlrigServer;
    picBallGreen: TBitmap;
    picBallBlue: TBitmap;
    TimerFormClose: TTimer;
  private
    procedure SetUpRig;
    procedure UpdateUI(Sender: TObject);
    procedure TimerFormCloseTimer(Sender: TObject);
    function RigFrequencyToCaption(frq: LongWord): String;

  public
    Rig: TRig;
  public
    procedure SetUp(restartTrx: Boolean; restartFlrig: Boolean);
  end;

var
  FormRig: TFormRig;

implementation

{$R *.lfm}

{ TFormRig }

procedure TFormRig.FormCreate(Sender: TObject);
begin
  Configuration:=TConfiguration.Create(Application.Location);
  Configuration.Load;

  SetUpRig;

  picBallGreen:=TBitMap.Create;
  picBallGreen.LoadFromResourceName(HInstance, 'BALL_GREEN_ICON');

  picBallBlue:=TBitMap.Create;
  picBallBlue.LoadFromResourceName(HInstance, 'BALL_BLUE_ICON');
end;

procedure TFormRig.FormShow(Sender: TObject);
begin
  NSView(Handle).window.setFrameAutosaveName(NSSTR(ClassName));

  GroupBox1.Enabled:=False;
  GroupBox2.Enabled:=False;
  GroupBox3.Enabled:=False;

  BitBtnDNF.Glyph.Assign(picBallBlue);
  BitBtnDNR.Glyph.Assign(picBallBlue);
  BitBtnVOX.Glyph.Assign(picBallBlue);
  BitBtnSplit.Glyph.Assign(picBallBlue);
  BitBtnTXW.Glyph.Assign(picBallBlue);

  Rig.Start;
  FlRigServer.Start;
  Application.ProcessMessages;

  FormDebug.Log('TFormRig: show');
end;

procedure TFormRig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TimerFormClose:=TTimer.Create(nil);
  TimerFormClose.Interval:=5;
  TimerFormClose.Enabled:=True;
  TimerFormClose.OnTimer:=@TimerFormCloseTimer;
end;

procedure TFormRig.TimerFormCloseTimer(Sender: TObject);
begin
  TimerFormClose.Enabled:=False;

  FlRigServer.Stop;
  Application.ProcessMessages;

  Rig.Stop;
  Application.ProcessMessages;
end;

procedure TFormRig.UpdateUI(Sender: TObject);
var
  frqDelta: Double;
  rpwr: Double;
begin
  BeginFormUpdate;

  Application.ProcessMessages;

  { rig is connected }
  if Rig.isActive then
  begin
    GroupBox1.Enabled:=True;
    GroupBox3.Enabled:=True;

    StatusBar1.Panels.Items[0].Text:='  ' + IntToStr(Rig.getBand) + 'M';
    StatusBar1.Panels.Items[1].Text:=Rig.getVersion;

    // VFO
    PanelVfoFrq.Caption:=RigFrequencyToCaption(Rig.getFrq);

    PanelVfoMode.Caption:=Rig.getMode;
    PanelVfoMode.Color:=clGreen;

    // RX buttons
    if Rig.voxActive
      then BitBtnVOX.Glyph.Assign(picBallGreen)
      else BitBtnVOX.Glyph.Assign(picBallBlue);

    if Rig.dnrActive
      then BitBtnDNR.Glyph.Assign(picBallGreen)
      else BitBtnDNR.Glyph.Assign(picBallBlue);

    if Rig.dnfActive
      then BitBtnDNF.Glyph.Assign(picBallGreen)
      else BitBtnDNF.Glyph.Assign(picBallBlue);

    if Rig.splitActive then
    begin
      frqDelta:=(Rig.getVfoB_frq - Rig.getVfoA_frq) / 1000;
      PanelVfoFrq.Color:=clYellow;
      PanelVfoFrq.Font.Color:=clBlack;
      PanelVfoAB.Color:=clYellow;
      PanelVfoAB.Font.Color:=clBlack;
      if frqDelta > 0
        then PanelVfoAB.Caption:= '+' + FloatToStr(frqDelta)
        else PanelVfoAB.Caption:= '-' + FloatToStr(Abs(frqDelta));
           BitBtnSplit.Glyph.Assign(picBallGreen);
      BitBtnSplitPlus.Enabled:=True;
      BitBtnSplitMinus.Enabled:=(Rig.getVfoB_frq > Rig.getVfoA_frq);
      BitBtnTXW.Enabled:=True;
      if Rig.txwActive then BitBtnTXW.Glyph.Assign(picBallGreen) else BitBtnTXW.Glyph.Assign(picBallBlue);
    end else
    begin
      PanelVfoFrq.Color:=clGreen;
      PanelVfoFrq.Font.Color:=clWhite;
      PanelVfoAB.Color:=clGreen;
      PanelVfoAB.Font.Color:=clWhite;
      case Rig.getVfo of
        RigVFO_A: PanelVfoAB.Caption:='A';
        RigVFO_B: PanelVfoAB.Caption:='B';
      end;
      BitBtnSplit.Glyph.Assign(picBallBlue);
      BitBtnSplitPlus.Enabled:=False;
      BitBtnSplitMinus.Enabled:=False;
      BitBtnTXW.Glyph.Assign(picBallBlue);
      BitBtnTXW.Enabled:=False;
    end;

    // pwr set on trx
    if Rig.getPwr <> TrackBarPwr.Position then begin
      TrackBarPwr.Enabled:=False;
      TrackBarPwr.Position:=Rig.getPwr;
      TrackBarPwr.Enabled:=True;
      LabelPwr.Caption:='Power ' + IntToStr(Rig.getPwr);
    end;

    // RPORT Gain aka DIGITAL Gain
    if Rig.getDrPortGain <> TrackBarDgain.Position  then begin
      TrackBarDgain.Enabled:=False;
      TrackBarDgain.Position:=Rig.getDrPortGain;
      TrackBarDgain.Enabled:=True;
      LabelDgain.Caption:='Digital gain ' + IntToStr(Rig.getDrPortGain);
    end;

    { RX }
    if not Rig.pttActive then
    begin
      GroupBox2.Enabled:=True;

      PanelPwr.Color:=clGray;
      PanelSwr.Color:=clGray;

      // s-meter
      PanelSmetr.Color:=clGreen;
      PanelSmetr.Caption:=Rig.getSMeter;
    end;

    { TX }
    if Rig.pttActive then
    begin
      GroupBox2.Enabled:=False;

      // pwr forwarded from trx
      PanelPwr.Color:=clGreen;
      PanelPwr.Caption:=IntToStr(Rig.getFPwrMax);

      // power reflected
      rpwr:=Rig.getRPwrAvgPercent;
      PanelSwr.Caption:=Format('%.1f', [rpwr]);
      if rpwr > 12 then PanelSwr.Color:=clRed
      else if rpwr >  1 then PanelSwr.Color:=clYellow
      else PanelSwr.Color:=clGreen;

      // dosable Smetr
      PanelSmetr.Caption:='0';
      PanelSmetr.Color:=clGray;
    end;
  end;

  { rig disconnected }
  if not Rig.isActive then begin
    StatusBar1.Panels.Items[1].Text:= 'Not connected';
    GroupBox1.Enabled:=False;
    GroupBox2.Enabled:=False;
    GroupBox3.Enabled:=False;
    PanelVfoFrq.Color:=clGray;
    PanelVfoFrq.Caption:='';
    PanelVfoAB.Color:=clGray;
    PanelVfoAB.Caption:='';
    PanelVfoMode.Color:=clGray;
    PanelVfoMode.Caption:='';
    PanelSmetr.Color:=clGray;
    PanelSmetr.Caption:='0';
    PanelSwr.Color:=clGray;
    PanelSwr.Caption:='0';
    PanelPwr.Color:=clGray;
    PanelPwr.Caption:='0';
  end;

  EndFormUpdate;
end;

procedure TFormRig.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ssCmd: TShiftStateEnum;
begin
  {$IFDEF DARWIN}
  ssCmd := ssMeta;
  {$ELSE}
  ssCmd := ssCtrl
  {$ENDIF}

  if Rig.isActive and (ssCmd in Shift) then begin
    case Key of
      VK_BACK: if BitBtnVOX.Enabled then BitBtnVOXClick(Sender);

      VK_S: if BitBtnSplit.Enabled then BitBtnSplitClick(Sender);
      VK_RETURN: if BitBtnTXW.Enabled then BitBtnTXWClick(Sender);
      VK_P: if BitBtnSplitPlus.Enabled then BitBtnSplitPlusClick(Sender);
      VK_L: if BitBtnSplitMinus.Enabled then BitBtnSplitMinusClick(Sender);

      VK_1: TrackBarPwr.Position:=10;
      VK_2: TrackBarPwr.Position:=20;
      VK_3: TrackBarPwr.Position:=30;
      VK_4: TrackBarPwr.Position:=40;
      VK_5: TrackBarPwr.Position:=50;
      VK_6: TrackBarPwr.Position:=60;
      VK_7: TrackBarPwr.Position:=70;
      VK_8: TrackBarPwr.Position:=80;
      VK_9: TrackBarPwr.Position:=90;
      VK_0: TrackBarPwr.Position:=100;
      VK_RIGHT: if (TrackBarPwr.Position < 100) then TrackBarPwr.Position:=TrackBarPwr.Position + 1;
      VK_LEFT: if (TrackBarPwr.Position > 5) then  TrackBarPwr.Position:=TrackBarPwr.Position - 1;

      VK_UP: if (TrackBarDgain.Position < 100) then TrackBarDgain.Position:=TrackBarDgain.Position + 5;
      VK_DOWN: if (TrackBarDgain.Position > 0) then TrackBarDgain.Position:=TrackBarDgain.Position - 5;
    end;
  end;
end;

procedure TFormRig.Button11Click(Sender: TObject);
begin
  if Rig.isActive then Rig.SetPwr(5);
end;

procedure TFormRig.Button12Click(Sender: TObject);
begin
 if Rig.isActive then Rig.SetPwr(25);
end;

procedure TFormRig.Button13Click(Sender: TObject);
begin
  if Rig.isActive then Rig.SetdrPortGain(30);
end;

procedure TFormRig.Button14Click(Sender: TObject);
begin
  if Rig.isActive then Rig.SetdrPortGain(50);
end;

procedure TFormRig.TrackBarPwrChange(Sender: TObject);
begin
  if Rig.isActive and TrackBarPwr.Enabled and (TrackBarPwr.Position >= 5) and (TrackBarPwr.Position <= 100) then Rig.SetPwr(TrackBarPwr.Position);
  LabelPwr.Caption:='Power ' + IntToStr(TrackBarPwr.Position);
end;

procedure TFormRig.TrackBarDgainChange(Sender: TObject);
begin
   if Rig.isActive and TrackBarDgain.Enabled and (TrackBarDgain.Position >= 0) and (TrackBarDgain.Position <= 100) then Rig.SetdrPortGain(TrackBarDgain.Position);
   LabelDgain.Caption:='Digital gain ' + IntToStr(TrackBarDgain.Position);
end;

procedure TFormRig.BitBtnDNFClick(Sender: TObject);
begin
  if Rig.isActive then Rig.toggleDnf;
end;

procedure TFormRig.BitBtnDNRClick(Sender: TObject);
begin
  if Rig.isActive then Rig.toggleDnr;
end;

procedure TFormRig.BitBtnVOXClick(Sender: TObject);
begin
  if Rig.isActive then Rig.toggleVox;
end;

procedure TFormRig.BitBtnSplitClick(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then
  begin
    if Rig.splitActive then begin
      { split off - set VFO B to VFO A}
      Rig.SetVfoB_frq(Rig.getVfoA_frq);
      Rig.SetSplit(False);
    end else begin
      { split on - set quick split +5 }
      Rig.SetVfoB_mode(Rig.getVfoA_mode);
      Rig.SetVfoB_frq(Rig.getVfoA_frq + 5000);
      Rig.SetSplit(True);
    end;
  end;
end;

procedure TFormRig.BitBtnSplitMinusClick(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetVfoB_frq(Rig.getVfoB_frq - 5000);
end;

procedure TFormRig.BitBtnSplitPlusClick(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetVfoB_frq(Rig.getVfoB_frq + 5000);
end;

procedure TFormRig.BitBtnTXWClick(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.toggleTxw;
end;

procedure TFormRig.Button1Click(Sender: TObject);
begin
   if Rig.isActive and not Rig.pttActive then Rig.SetBand_40;
end;

procedure TFormRig.Button10Click(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetBand_80;
end;

procedure TFormRig.Button2Click(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetBand_30
end;

procedure TFormRig.Button3Click(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetBand_20
end;

procedure TFormRig.Button4Click(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetBand_17
end;

procedure TFormRig.Button5Click(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetBand_15
end;

procedure TFormRig.Button6Click(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetBand_12
end;

procedure TFormRig.Button7Click(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetBand_10
end;

procedure TFormRig.Button8Click(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetBand_160
end;

procedure TFormRig.Button9Click(Sender: TObject);
begin
  if Rig.isActive and not Rig.pttActive then Rig.SetBand_6
end;

{ helpers }

function TFormRig.RigFrequencyToCaption(frq: LongWord): String;
var
  frequency: String;
begin
  if frq > 0 then begin
   frequency:=PadLeft(IntToStr(Rig.getFrq), 9);
   Result:=Trim(frequency.Substring(0, 3)) + '.' + frequency.Substring(3, 3) + '.' + frequency.Substring(6, 3);
  end else
   Result:='';
end;

procedure TFormRig.SetUpRig;
begin
  case Configuration.Settings.Trx of
    TSettingsTrx.FTDX10: Rig:=TFTdx10rig.Create(Configuration);
    TSettingsTrx.FT991A: Rig:=TFT991Arig.Create(Configuration);
    else Rig:=TRigDummy.Create;
  end;

  Rig.onState(@UpdateUI);

  flRigServer:=TFlrigServer.Create(Configuration, Rig);

  Caption:=Configuration.Settings.trx;
end;

procedure TFormRig.SetUp(restartTrx: Boolean; restartFlrig: Boolean);
begin
  Configuration.Load;

  if restartTrx then SetUpRig;

  if restartFlrig then FlRigServer.Stop;
  if restartTrx then Rig.Stop;
  if restartTrx then Rig.Start;
  if restartFlrig then FlRigServer.Start;
end;

end.

