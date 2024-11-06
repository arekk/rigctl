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
  UnitFormDebug,
  UnitFlrigServer,
  UnitSettings,
  UnitRig;

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
    ImageBallBlue: TImage;
    ImageBallGreen: TImage;
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
    Timer1: TTimer;
    TrackBarPwr: TTrackBar;
    TrackBarDgain: TTrackBar;
    procedure BitBtnSplitClick(Sender: TObject);
    procedure BitBtnSplitMinusClick(Sender: TObject);
    procedure BitBtnSplitPlusClick(Sender: TObject);
    procedure BitBtnTXWClick(Sender: TObject);
    procedure ReloadConfiguration;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBarPwrChange(Sender: TObject);
    procedure TrackBarDgainChange(Sender: TObject);
  private
    function RigFrequencyToCaption(frq: LongWord): String;
  public
    Rig: TRig;
  private
    Configuration: TConfiguration;
    FlRigServer: TFlrigServer;
    picBallGreen: TBitmap;
    picBallBlue: TBitmap;
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

   Visible:=Configuration.Settings.trxEnabled;

   Rig:=TRig.Create(Configuration);

   FlRigServer:=TFlrigServer.Create(Configuration);

   picBallGreen:=ImageBallGreen.Picture.Bitmap;
   picBallBlue:=ImageBallBlue.Picture.Bitmap;
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

  FlRigServer.Rig:=Rig;
  FlRigServer.Start;

  Timer1.Interval:=Configuration.Settings.trxPool;
  Timer1.Enabled:=True;
end;

procedure TFormRig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FlRigServer.Stop;
  Rig.Stop;
  CloseAction:=caHide;
end;

procedure TFormRig.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FlRigServer);
  FreeAndNil(Rig);
  FreeAndNil(Configuration);
end;

procedure TFormRig.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE: if BitBtnVOX.Enabled then BitBtnVOXClick(Sender);
    VK_S: if BitBtnSplit.Enabled then BitBtnSplitClick(Sender);
    VK_T: if BitBtnTXW.Enabled then BitBtnTXWClick(Sender);
    VK_I: if BitBtnSplitPlus.Enabled then BitBtnSplitPlusClick(Sender);
    VK_J: if BitBtnSplitMinus.Enabled then BitBtnSplitMinusClick(Sender);
  end;

  if Shift = [ssShift] then begin
    case Key of
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

procedure TFormRig.Button1Click(Sender: TObject);
begin
   Rig.SetBand(Rig.BAND_40)
end;

procedure TFormRig.Button10Click(Sender: TObject);
begin
  Rig.SetBand(Rig.BAND_80)
end;

procedure TFormRig.Button11Click(Sender: TObject);
begin
  Rig.SetPwr(5);
end;

procedure TFormRig.Button12Click(Sender: TObject);
begin
 Rig.SetPwr(25);
end;

procedure TFormRig.Button13Click(Sender: TObject);
begin
  Rig.SetdrPortGain(30);
end;

procedure TFormRig.Button14Click(Sender: TObject);
begin
  Rig.SetdrPortGain(50);
end;

procedure TFormRig.TrackBarPwrChange(Sender: TObject);
begin
  if (TrackBarPwr.Position >= 5) and (TrackBarPwr.Position <= 100) then Rig.SetPwr(TrackBarPwr.Position);
  LabelPwr.Caption:='Power ' + IntToStr(TrackBarPwr.Position);
end;

procedure TFormRig.TrackBarDgainChange(Sender: TObject);
begin
   if (TrackBarDgain.Position >= 0) and (TrackBarDgain.Position <= 100) then Rig.SetdrPortGain(TrackBarDgain.Position);
   LabelDgain.Caption:='Digital gain ' + IntToStr(TrackBarDgain.Position);
end;

procedure TFormRig.BitBtnDNFClick(Sender: TObject);
begin
  Rig.toggleDnf;
end;

procedure TFormRig.BitBtnDNRClick(Sender: TObject);
begin
  Rig.toggleDnr;
end;

procedure TFormRig.BitBtnVOXClick(Sender: TObject);
begin
  Rig.toggleVox;
end;

procedure TFormRig.BitBtnSplitClick(Sender: TObject);
begin
  if Rig.state.split then begin
    { split off - set VFO B to VFO A}
    Rig.SetVfoB_frq(Rig.state.vfoA_frq);
    Rig.SetSplit(False);
  end else begin
    { split on - set quick split +5 }
    Rig.SetVfoB_mode(Rig.state.vfoA_mode);
    Rig.SetVfoB_frq(Rig.state.vfoA_frq + 5000);
    Rig.SetSplit(True);
  end;
end;

procedure TFormRig.BitBtnSplitMinusClick(Sender: TObject);
begin
  Rig.SetVfoB_frq(Rig.state.vfoB_frq - 5000);
end;

procedure TFormRig.BitBtnSplitPlusClick(Sender: TObject);
begin
  Rig.SetVfoB_frq(Rig.state.vfoB_frq + 5000);
end;

procedure TFormRig.BitBtnTXWClick(Sender: TObject);
begin
  Rig.toggleTxw;
end;

procedure TFormRig.Button2Click(Sender: TObject);
begin
  Rig.SetBand(Rig.BAND_30)
end;

procedure TFormRig.Button3Click(Sender: TObject);
begin
  Rig.SetBand(Rig.BAND_20)
end;

procedure TFormRig.Button4Click(Sender: TObject);
begin
  Rig.SetBand(Rig.BAND_17)
end;

procedure TFormRig.Button5Click(Sender: TObject);
begin
  Rig.SetBand(Rig.BAND_15)
end;

procedure TFormRig.Button6Click(Sender: TObject);
begin
  Rig.SetBand(Rig.BAND_12)
end;

procedure TFormRig.Button7Click(Sender: TObject);
begin
  Rig.SetBand(Rig.BAND_10)
end;

procedure TFormRig.Button8Click(Sender: TObject);
begin
  Rig.SetBand(Rig.BAND_160)
end;

procedure TFormRig.Button9Click(Sender: TObject);
begin
  Rig.SetBand(Rig.BAND_6)
end;

function TFormRig.RigFrequencyToCaption(frq: LongWord): String;
var
  frequency: String;
begin
  if frq > 0 then begin
   frequency:=PadLeft(IntToStr(Rig.state.frq), 9);
   Result:=Trim(frequency.Substring(0, 3)) + '.' + frequency.Substring(3, 3) + '.' + frequency.Substring(6, 3);
  end else
   Result:='';
end;

procedure TFormRig.Timer1Timer(Sender: TObject);
begin
  { rig is connected }
  if Rig.state.active then
  begin
     GroupBox1.Enabled:=True;
     GroupBox2.Enabled:=True;
     GroupBox3.Enabled:=True;

     // VFO
     PanelVfoFrq.Caption:=RigFrequencyToCaption(Rig.state.frq);
     PanelVfoMode.Caption:=Rig.state.mode;
     case Rig.state.vfo of
       0: PanelVfoAB.Caption:='A';
       1: PanelVfoAB.Caption:='B';
     end;

     // RX buttons
     if Rig.state.vox
       then BitBtnVOX.Glyph.Assign(picBallGreen)
       else BitBtnVOX.Glyph.Assign(picBallBlue);

     if Rig.state.dnr
       then BitBtnDNR.Glyph.Assign(picBallGreen)
       else BitBtnDNR.Glyph.Assign(picBallBlue);

     if Rig.state.dnf
       then BitBtnDNF.Glyph.Assign(picBallGreen)
       else BitBtnDNF.Glyph.Assign(picBallBlue);

     if Rig.state.split then
     begin
       BitBtnSplit.Glyph.Assign(picBallGreen);
       BitBtnSplitPlus.Enabled:=True;
       BitBtnSplitMinus.Enabled:=(Rig.state.vfoB_frq > Rig.state.vfoA_frq);
       BitBtnTXW.Enabled:=True;
       if Rig.state.txw then BitBtnTXW.Glyph.Assign(picBallGreen) else BitBtnTXW.Glyph.Assign(picBallBlue);
     end else
     begin
       BitBtnSplit.Glyph.Assign(picBallBlue);
       BitBtnSplitPlus.Enabled:=False;
       BitBtnSplitMinus.Enabled:=False;
       BitBtnTXW.Glyph.Assign(picBallBlue);
       BitBtnTXW.Enabled:=False;
     end;

     // pwr set on trx
     if Rig.state.pwr <> TrackBarPwr.Position then begin
       TrackBarPwr.Enabled:=False;
       TrackBarPwr.Position:=Rig.state.pwr;
       TrackBarPwr.Enabled:=True;
       LabelPwr.Caption:='Power ' + IntToStr(Rig.state.pwr);
     end;

     // RPORT Gain aka DIGITAL Gain
     if Rig.state.drPortGain <> TrackBarDgain.Position  then begin
       TrackBarDgain.Enabled:=False;
       TrackBarDgain.Position:=Rig.state.drPortGain;
       TrackBarDgain.Enabled:=True;
       LabelDgain.Caption:='Digital gain ' + IntToStr(Rig.state.drPortGain);
     end;

     { RX }
     if not Rig.state.ptt then
     begin
       PanelPwr.Color:=clGray;
       PanelSwr.Color:=clGray;

       // s-meter
       PanelSmetr.Color:=clGreen;
       PanelSmetr.Caption:=Rig.state.getSMeter;

       // vfo
       PanelVfoMode.Color:=clGreen;

       if Rig.state.split then begin
         PanelVfoFrq.Color:=clYellow;
         PanelVfoFrq.Font.Color:=clBlack;
         PanelVfoAB.Color:=clYellow;
         PanelVfoAB.Font.Color:=clBlack;
       end else begin
         PanelVfoFrq.Color:=clGreen;
         PanelVfoFrq.Font.Color:=clWhite;
         PanelVfoAB.Color:=clGreen;
         PanelVfoAB.Font.Color:=clWhite;
       end;
     end;

     { TX }
     if Rig.state.ptt then
     begin
       // pwr forwarded from trx
       PanelPwr.Color:=clRed;
       PanelPwr.Caption:=IntToStr(Rig.state.getFPwrMax);

       // power reflected
       PanelSwr.Color:=clGreen;
       PanelSwr.Caption:=Format('%.1f', [Rig.state.getRPwrAvgPercent]);

       PanelSmetr.Caption:='0';
       PanelSmetr.Color:=clGray;
     end;
   end;

  { rig disconnected }
  if not Rig.state.active then begin
    GroupBox1.Enabled:=False;
    GroupBox2.Enabled:=False;
    GroupBox3.Enabled:=False;
    PanelVfoFrq.Color:=clGray;
    PanelVfoFrq.Caption:='';
    PanelVfoMode.Color:=clGray;
    PanelVfoMode.Caption:='';
    PanelSmetr.Color:=clGray;
    PanelSmetr.Caption:='0';
    PanelSwr.Color:=clGray;
    PanelSwr.Caption:='0';
    PanelPwr.Color:=clGray;
    PanelPwr.Caption:='0';
  end;
end;

procedure TFormRig.ReloadConfiguration;
begin
   Configuration.Load;
   FlRigServer.Stop;
   Rig.Stop;
   Rig.Start;
   FlRigServer.Start;
end;

end.

