unit UnitFormSpert;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes,
  SysUtils,
  Dateutils,
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
  UnitSpertDatabase,
  UnitSpert,
  UnitFormRig;

type
  { TFormSpert }

  TFormSpert = class(TForm)
    ButtonAtuTune: TBitBtn;
    BitBtnAtu: TBitBtn;
    BitBtnLockTx: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PanelAtuLC: TPanel;
    PanelTemp: TPanel;
    PanelPavg: TPanel;
    PanelPref: TPanel;
    PanelAtuSWR: TPanel;
    PanelPmax: TPanel;
    PanelPcur: TPanel;
    StatusBar1: TStatusBar;
    TrackBar1: TTrackBar;
    procedure BitBtnLockTxClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure BitBtnAtuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ButtonAtuTuneClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1MouseEnter(Sender: TObject);
    procedure TrackBar1MouseLeave(Sender: TObject);
    private
      Configuration: TConfiguration;
      TimerFormClose: TTimer;
      picBallGreen: TBitmap;
      picBallBlue: TBitmap;
    private
      procedure UpdateUI(Sender: TObject);
      procedure TimerFormCloseTimer(Sender: TObject);
    public
      Spert: TSpert;
      SpertDatabase: TSpertDatabase;
    public
      procedure SetUp(restart: Boolean);
    const
      MAX_GREEN_SWR = 1;
  end;

var
  FormSpert: TFormSpert;

implementation

{$R *.lfm}

{ TFormSpert }

procedure TFormSpert.FormCreate(Sender: TObject);
begin
  Configuration:=TConfiguration.Create(Application.Location);
  Configuration.Load;

  Spert:=TSpert.Create(Configuration);
  Spert.onState(@UpdateUI);

  SpertDatabase:=TSpertDatabase.Create(Configuration);

  picBallGreen:=TBitMap.Create;
  picBallGreen.LoadFromResourceName(HInstance, 'BALL_GREEN_ICON');

  picBallBlue:=TBitMap.Create;
  picBallBlue.LoadFromResourceName(HInstance, 'BALL_BLUE_ICON');
end;

procedure TFormSpert.FormShow(Sender: TObject);
begin
  NSView(Handle).window.setFrameAutosaveName(NSSTR(ClassName));

  GroupBox1.Enabled:=False;
  GroupBox2.Enabled:=False;
  StatusBar1.Panels.Items[2].Text:= 'Not connected';
  BitBtnLockTx.Glyph.Assign(picBallGreen);
  BitBtnAtu.Glyph.Assign(picBallBlue);

  SpertDatabase.Open;

  Spert.Start;

  Application.ProcessMessages;
end;

procedure TFormSpert.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Spert will be stopped into background because it's slow and delaying form close
  TimerFormClose:=TTimer.Create(nil);
  TimerFormClose.Interval:=10;
  TimerFormClose.Enabled:=True;
  TimerFormClose.OnTimer:=@TimerFormCloseTimer;

  CloseAction:=caHide;
end;

procedure TFormSpert.TimerFormCloseTimer(Sender: TObject);
begin
  TimerFormClose.Enabled:=False;

  Spert.Stop;
  Application.ProcessMessages;

  SpertDatabase.Close;
end;

procedure TFormSpert.UpdateUI(Sender: TObject);
var
  Swr: Double;
begin
  BeginFormUpdate;

  Application.ProcessMessages;

  if Spert.isActive then
  begin
    GroupBox1.Enabled:=True;

    // power
    Label1.Caption:='Power ' + IntToStr(Spert.getPwr * 10) + ' W';

    if (TrackBar1.Tag = 0) and (Spert.getPwr <> TrackBar1.Position) then
    begin
      TrackBar1.Enabled:=false;
      TrackBar1.Position:=Spert.getPwr;
      TrackBar1.Enabled:=true;
    end;

    // band
    if Spert.getBand <> StatusBar1.Panels.Items[0].Text then StatusBar1.Panels.Items[0].Text:=Spert.getBand;

    // temp
    PanelTemp.Caption:=IntToStr(Spert.getTemp);

    if (Spert.getTemp > 0) AND (Spert.getTemp < 40) then
    begin
      PanelTemp.Color:=clGreen;
      PanelTemp.Font.Color:=clWhite;
    end;
    if (Spert.getTemp >= 40) AND (Spert.getTemp < 45) then
    begin
      PanelTemp.Color:=clYellow;
      PanelTemp.Font.Color:=clBlack;
    end;
    if (Spert.getTemp >= 45) then
    begin
      PanelTemp.Color:=clRed;
      PanelTemp.Font.Color:=clWhite;
    end;

    // TX
    if Spert.txActive then
    begin
      PanelPavg.Color:=clGreen;
      PanelPavg.Caption:=IntToStr(Spert.getFpwrAvg);

      PanelPmax.Color:=clGreen;
      PanelPmax.Caption:=IntToStr(Spert.getFpwrMax);

      PanelPcur.Color:=clGreen;
      PanelPcur.Caption:=IntToStr(Spert.getFpwr);

      PanelPref.Caption:=Format('%.1f', [Spert.getRpwrMaxPercent]);

      if Spert.getRpwrMaxPercent < 1 then
      begin
        PanelPref.Color:=clGreen;
        PanelPref.Font.Color:=clWhite;
      end;
      if (Spert.getRpwrMaxPercent >= 1) and (Spert.getRpwrMaxPercent < 5) then
      begin
        PanelPref.Color:=clYellow;
        PanelPref.Font.Color:=clBlack;
      end;
      if (Spert.getRpwrMaxPercent >= 5) then
      begin
        PanelPref.Color:=clRed;
        PanelPref.Font.Color:=clWhite;
      end;

      GroupBox1.Enabled:=False;
      GroupBox2.Enabled:=False;
    end;

    if not Spert.txActive then
    begin
      PanelPavg.Color:=clGray;
      PanelPmax.Color:=clGray;
      PanelPcur.Color:=clGray;

      if PanelPref.Color=clGreen then PanelPref.Color:=clGray; // set back to gray if there was no excessive reflected power

      GroupBox1.Enabled:=True;
      GroupBox2.Enabled:=True;
    end;

    // atu presence
    GroupBox2.Enabled:=SPert.isAtuPresent;

    // atu is starting to tune
    if Spert.isAtuTunning then
    begin
      BitBtnAtu.Enabled:=False;
      ButtonAtuTune.Enabled:=False;
      PanelAtuLC.Color:=clMaroon;
    end;

    // atu is not tunning - update state
    if not Spert.isAtuTunning then
    begin
      BitBtnAtu.Enabled:=True;

      // atu is on
      if Spert.isAtuActive then
      begin
        BitBtnAtu.Glyph.Assign(picBallGreen);
        ButtonAtuTune.Enabled:=True;
      end
      else
      begin
        BitBtnAtu.Glyph.Assign(picBallBlue);
        ButtonAtuTune.Enabled:=False;
      end;
    end;

    // fan
    case Spert.getFan of
      SpertFanLevel_50:         StatusBar1.Panels.Items[1].Text:='50%';
      SpertFanLevel_Fixed:      StatusBar1.Panels.Items[1].Text:='Fixed';
      SpertFanLevel_Histeresis: StatusBar1.Panels.Items[1].Text:='Histeresis';
      SpertFanLevel_Max:        StatusBar1.Panels.Items[1].Text:='Max';
    end;

    // version
    StatusBar1.Panels.Items[2].Text:=Spert.getVersion;

    { SWR - interact with PanelAtuSWR/PanelAtuLC }

    if not FormRig.Rig.isActive then
    begin
      if Spert.isTxLocked then Spert.txLockOff; // unlock TX if radio goes off
      BitBtnLockTx.Enabled:=True;

      // PanelAtuSWR is gray and shows nothing
      PanelAtuSWR.Caption:='';
      PanelAtuSWR.Color:=clGray;

      // PanelAtuLC is always gray - SPert colors don't works as expected and it's "green" do not mean "ATU is tunned"
      if not Spert.isAtuTunning then PanelAtuLC.Color:=clGray;

      if Spert.isAtuActive and not Spert.isAtuTunning
        then PanelAtuLC.Caption:=FloatToStr(Spert.getAtu_L) + 'μH ' + FloatToStr(Spert.getAtu_C) + 'pF'
        else PanelAtuLC.Caption:='';
    end;

    if FormRig.Rig.isActive then
    begin
      // PanelAtuSWR shows sawed SWR and if ATU is off use colors to prompt what to do, id ATU is on is always green
      Swr:=SpertDatabase.estimateSwr(FormRig.Rig.getFrq, 3000); // +-3kHz
      if Swr = -1 then
      begin
        Swr:=SpertDatabase.estimateSwr(FormRig.Rig.getFrq, 30000); // +-30kHz
         if Swr = -1
           then PanelAtuSWR.Caption:=''
           else PanelAtuSWR.Caption:='~' + Format('%.1f', [Swr])
      end else
        PanelAtuSWR.Caption:=Format('%.1f', [Swr]);

      if Spert.isAtuActive
        then PanelAtuSWR.Color:=clGreen;

      if not Spert.isAtuActive then
      begin
        if (Swr = -1)
          then PanelAtuSWR.Color:=clGray;
        if (Swr >= 0) and (Swr <= MAX_GREEN_SWR)
          then PanelAtuSWR.Color:=clGreen;
        if (Swr > MAX_GREEN_SWR)
          then PanelAtuSWR.Color:=clRed;
      end;

      // PanelAtuLC if ATU is on use saved LC to prompt ATU re-tune, if ATU is off it's gray and shows nothing
      if Spert.isAtuActive and not Spert.isAtuTunning then
      begin
        if (Swr = -1) or (Swr > MAX_GREEN_SWR) then
          begin
            case SpertDatabase.matchLC(FormRig.Rig.getFrq, 10000, Spert.getAtu_L, Spert.getAtu_C) of
            SpertDatabaseLC_Unknown: PanelAtuLC.Color:=clGray;
            SpertDatabaseLC_Match: PanelAtuLC.Color:=clGreen;
            SpertDatabaseLC_NotMatch: PanelAtuLC.Color:=clRed;
          end;
        end
          else PanelAtuLC.Color:=clGreen;

        PanelAtuLC.Caption:=FloatToStr(Spert.getAtu_L) + 'μH ' + FloatToStr(Spert.getAtu_C) + 'pF';
      end
      else
      begin
        PanelAtuLC.Color:=clGray;
        PanelAtuLC.Caption:='';
      end;

      if (BitBtnLockTx.Tag = 0) and not Spert.isAtuActive then
      begin
        // if ATU is not active and SWR is too high lock SPert and disable possibility unlocking it
        if (Configuration.Settings.spertLockOnSwr > 0) and (Swr >= Configuration.Settings.spertLockOnSwr) and not Spert.isTxLocked then
        begin
           Spert.txLockOn;
           BitBtnLockTx.Enabled:=False;
        end;
        if (Configuration.Settings.spertLockOnSwr > 0) and (Swr < Configuration.Settings.spertLockOnSwr) and Spert.isTxLocked then
        begin
          Spert.txLockOff;
          BitBtnLockTx.Enabled:=True;
        end;
      end;

      // unlock SPert if ATU is on, but ignore it if lock manually turned on
      if (BitBtnLockTx.Tag = 0) and Spert.isAtuActive and Spert.isTxLocked then
      begin
        Spert.txLockOff;
        BitBtnLockTx.Enabled:=True;
      end;
    end;

    // lock button
    if Spert.isTxLocked then BitBtnLockTx.Glyph.Assign(picBallBlue) else BitBtnLockTx.Glyph.Assign(picBallGreen);
  end;

  if not Spert.isActive then
  begin
    GroupBox1.Enabled:=False;
    GroupBox2.Enabled:=False;

    StatusBar1.Panels.Items[0].Text:='';
    StatusBar1.Panels.Items[1].Text:='';
    StatusBar1.Panels.Items[2].Text:= 'Not connected';

    PanelAtuSWR.Color:=clGray;
    PanelAtuLC.Color:=clGray;

    PanelTemp.Color:=clGray;
    PanelTemp.Font.Color:=clWhite;
    PanelTemp.Caption:='0';

    PanelPavg.Color:=clGray;
    PanelPavg.Caption:='0';

    PanelPref.Color:=clGray;
    PanelPref.Font.Color:=clWhite;
    PanelPref.Caption:='0.0';

    PanelPmax.Color:=clGray;
    PanelPmax.Caption:='0';

    PanelPcur.Color:=clGray;
    PanelPcur.Caption:='0';
  end;

  EndFormUpdate;
end;

{ global keyboard shortcuits}
procedure TFormSpert.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ssCmd: TShiftStateEnum;
begin
  {$IFDEF DARWIN}
  ssCmd := ssMeta;
  {$ELSE}
  ssCmd := ssCtrl
  {$ENDIF}

  if (ssCmd in Shift) then begin
    if Spert.isActive and not Spert.txActive and BitBtnAtu.Enabled and (Key = VK_A) then BitBtnAtuClick(Sender);
    if Spert.isActive and not Spert.txActive and ButtonAtuTune.Enabled and (Key = VK_T) then ButtonAtuTuneClick(Sender);
    if Spert.isActive and not Spert.txActive and TrackBar1.Enabled then
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
end;

{ power: fixed 100W }
procedure TFormSpert.Button1Click(Sender: TObject);
begin
  if Spert.isActive and not Spert.txActive and TrackBar1.Enabled
    then TrackBar1.Position:=10;
end;

{ power: fixed 300W }
procedure TFormSpert.Button5Click(Sender: TObject);
begin
  if Spert.isActive and not Spert.txActive and TrackBar1.Enabled
    then TrackBar1.Position:=30;
end;

procedure TFormSpert.BitBtnLockTxClick(Sender: TObject);
begin
  if Spert.isTxLocked then
  begin
    Spert.txLockOff;
    BitBtnLockTx.Glyph.Assign(picBallGreen);
    BitBtnLockTx.Tag:=0;
  end
  else
  begin
    BitBtnLockTx.Glyph.Assign(picBallBlue);
    BitBtnLockTx.Tag:=1;
    Spert.txLockOn;
  end;
end;

{ power: fixed 500W }
procedure TFormSpert.Button2Click(Sender: TObject);
begin
  if Spert.isActive and not Spert.txActive and TrackBar1.Enabled
    then TrackBar1.Position:=50;
end;

{ power: fixed 700W }
procedure TFormSpert.Button3Click(Sender: TObject);
begin
  if Spert.isActive and not Spert.txActive and TrackBar1.Enabled
    then TrackBar1.Position:=70;
end;

{ ATU on/by-pass }
procedure TFormSpert.BitBtnAtuClick(Sender: TObject);
begin
  if Spert.isActive and not Spert.txActive and BitBtnAtu.Enabled then SPert.toggleAtuState;
end;

{ ATU tune }
procedure TFormSpert.ButtonAtuTuneClick(Sender: TObject);
begin
  if Spert.isActive and not Spert.txActive and ButtonAtuTune.Enabled then
  begin
    Spert.tuneAtu;
    ButtonAtuTune.Enabled:=False;
  end;
end;

{ PWR slider }
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
  if Spert.isActive and not Spert.txActive and TrackBar1.Enabled and (TrackBar1.Position >= 0) and (TrackBar1.Position <= 100) then Spert.setPower(TrackBar1.Position);
end;

{ helpers }

procedure TFormSpert.SetUp(restart: Boolean);
begin
  Configuration.Load;
  if restart then
  begin
    Spert.Stop;
    Spert.Start;
  end;
end;

end.
