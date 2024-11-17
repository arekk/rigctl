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
  SQLdb,
  SQLite3dyn,
  SQLite3Conn,
  UnitSettings,
  UnitRig,
  UnitSpert,
  UnitFormDebug;

type
  { TFormSpert }

  TFormSpert = class(TForm)
      BitBtnLockTx: TBitBtn;
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
    PanelATUColor: TPanel;
    PanelTemp: TPanel;
    PanelPavg: TPanel;
    PanelPref: TPanel;
    PanelATUState: TPanel;
    PanelPmax: TPanel;
    PanelPcur: TPanel;
    StatusBar1: TStatusBar;
    TimerState: TTimer;
    TimerCleanup: TTimer;
    TrackBar1: TTrackBar;

    procedure BitBtnLockTxClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure TimerCleanupTimer(Sender: TObject);
    procedure TimerStateTimer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1MouseEnter(Sender: TObject);
    procedure TrackBar1MouseLeave(Sender: TObject);

    private
      Configuration: TConfiguration;
      txTicks: Integer;
      txWas: Boolean;
      databaseConnection: TSQLite3Connection;
      databaseTransaction: TSQLTransaction;
      databaseQuery: TSQLQuery;
      picBallGreen: TBitmap;
      picBallBlue: TBitmap;
    private
      procedure setUpDatabase;
      function estimateSwr(span: Integer): Double;
      procedure insertSwr;
    public
      Spert: TSpert;
      Rig: TRig;
    public
      procedure ReloadConfiguration(restart: Boolean);
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

  databaseTransaction:=TSQLTransaction.Create(nil);
  databaseConnection:=TSQLite3Connection.Create(nil);
  databaseConnection.Transaction:=databaseTransaction;
  databaseConnection.DatabaseName:=Configuration.getConfigDirectory + PathDelim + 'spert.db';
  databaseQuery:=TSQLQuery.Create(nil);
  databaseQuery.Transaction:=databaseTransaction;
  databaseQuery.Database := databaseConnection;

  picBallGreen:=TBitMap.Create;
  picBallGreen.LoadFromResourceName(HInstance, 'BALL_GREEN_ICON');
  picBallBlue:=TBitMap.Create;
  picBallBlue.LoadFromResourceName(HInstance, 'BALL_BLUE_ICON');

  txTicks:=0;
  txWas:=False;
end;

procedure TFormSpert.FormShow(Sender: TObject);
begin
  NSView(Handle).window.setFrameAutosaveName(NSSTR(ClassName));

  GroupBox1.Enabled:=False;
  GroupBox2.Enabled:=False;
  StatusBar1.Panels.Items[2].Text:= 'Not connected';
  BitBtnLockTx.Glyph.Assign(picBallGreen);

  databaseConnection.Open;
  setUpDatabase;

  Spert.Start;

  TimerState.Interval:=Configuration.Settings.spertPool;
  TimerState.Enabled:=True;
end;

procedure TFormSpert.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Spert.Stop;
  databaseConnection.Close;
  CloseAction:=caHide;
end;

procedure TFormSpert.FormDestroy(Sender: TObject);
begin
  Spert.Stop;
  databaseConnection.Close;
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
    if Spert.isActive and not Spert.txActive and Button6.Enabled and (Key = VK_A) then Button6Click(Sender);
    if Spert.isActive and not Spert.txActive and Button4.Enabled and (Key = VK_T) then Button4Click(Sender);
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
procedure TFormSpert.Button6Click(Sender: TObject);
begin
  if Spert.isActive and not Spert.txActive and Button6.Enabled then SPert.toggleAtuState;
end;

{ ATU tune }
procedure TFormSpert.Button4Click(Sender: TObject);
begin
  if Spert.isActive and not Spert.txActive and Button4.Enabled then
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
  if Spert.isActive and not Spert.txActive and TrackBar1.Enabled and (TrackBar1.Position >= 0) and (TrackBar1.Position <= 100) then Spert.setPower(TrackBar1.Position);
end;

procedure TFormSpert.TimerStateTimer(Sender: TObject);
var
  Swr: Double;
begin
  BeginFormUpdate;

  if Spert.isActive then
  begin
    GroupBox1.Enabled:=True;

    // power
    if (TrackBar1.Tag = 0) and (Spert.getPwr <> TrackBar1.Position) then
    begin
      TrackBar1.Enabled:=false;
      TrackBar1.Position:=Spert.getPwr;
      TrackBar1.Enabled:=true;
    end;

    Label1.Caption:='Power ' + IntToStr(Spert.getPwr * 10) + ' W';

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

    { TX }

    if Spert.txActive then
    begin
      PanelPavg.Color:=clGreen;
      PanelPavg.Caption:=IntToStr(Spert.getFpwrAvg);

      PanelPmax.Color:=clGreen;
      PanelPmax.Caption:=IntToStr(Spert.getFpwrMax);

      PanelPcur.Color:=clGreen;
      PanelPcur.Caption:=IntToStr(Spert.getFpwrCur);

      PanelPref.Caption:=Format('%.1f', [Spert.getRpwrAvgPercent]);

      if Spert.getRpwrAvgPercent < 1 then
      begin
        PanelPref.Color:=clGreen;
        PanelPref.Font.Color:=clWhite;
      end;
      if (Spert.getRpwrAvgPercent >= 1) and (Spert.getRpwrAvgPercent < 5) then
      begin
        PanelPref.Color:=clYellow;
        PanelPref.Font.Color:=clBlack;
      end;
      if (Spert.getRpwrAvgPercent >= 5) then
      begin
        PanelPref.Color:=clRed;
        PanelPref.Font.Color:=clWhite;
      end;

      GroupBox1.Enabled:=False;
      GroupBox2.Enabled:=False;

      TimerCleanup.Enabled:=False;
    end;

    if not Spert.txActive then
    begin
      PanelPavg.Color:=clGray;
      PanelPmax.Color:=clGray;
      PanelPcur.Color:=clGray;

      if PanelPref.Color=clGreen then PanelPref.Color:=clGray; // set back to gray if there was no excessive reflected power

      GroupBox1.Enabled:=True;
      GroupBox2.Enabled:=True;

      if txWas then TimerCleanup.Enabled:=True;
    end;

    { ATU }

    // atu presence
    GroupBox2.Enabled:=SPert.isAtuPresent;

    // atu is starting to tune
    if Spert.isAtuTunning then
    begin
      Button6.Enabled:=False;
      PanelATUColor.Color:=clMaroon;
      Button4.Enabled:=False;
    end;

    // atu is not tunning - update state
    if not Spert.isAtuTunning then begin

      // atu is on
      if Spert.isAtuActive then
      begin
        PanelATUState.Color:=clGreen;

        if Spert.getAtuColor = 'GREY' then PanelATUColor.Color:=clGray;
        if Spert.getAtuColor = 'RED' then PanelATUColor.Color:=clRed;
        if Spert.getAtuColor = 'GREEN' then PanelATUColor.Color:=clGreen;

        Button6.Enabled:=True;
        Button4.Enabled:=True;
      end;

      // atu is in by-pass state (off)
      if not Spert.isAtuActive then
      begin
        PanelATUState.Color:=clGray;
        PanelATUColor.Color:=clGray;
        Button4.Enabled:=False;
      end;
    end;

    // current fan settings
    case Spert.getFan of
      SpertFanLevel_50:         StatusBar1.Panels.Items[1].Text:='50%';
      SpertFanLevel_Fixed:      StatusBar1.Panels.Items[1].Text:='Fixed';
      SpertFanLevel_Histeresis: StatusBar1.Panels.Items[1].Text:='Histeresis';
      SpertFanLevel_Max:        StatusBar1.Panels.Items[1].Text:='Max';
    end;

    // version
    StatusBar1.Panels.Items[2].Text:=Spert.getVersion;

    { SWR }

    // count loops when tx was enabled to estimate how long amp was in TX state - it's required to determine should swr be inserted into database
    if Assigned(Rig) and Rig.isActive and not Spert.isAtuActive and Spert.txActive then Inc(txTicks);

    // save swr into database
    if Assigned(Rig) and Rig.isActive and not Spert.isAtuActive and txWas and not Spert.txActive and (txTicks > 5)  then
    begin
      insertSwr;
      txTicks:=0;
    end;

    // show estimated swr
    Swr:=estimateSwr(3000); // +-3kHz
    if Swr = -1 then
    begin
      Swr:=estimateSwr(30000); // +-30kHz
       if Swr = -1
         then PanelATUState.Caption:='¯\_(ツ)_/¯'
         else PanelATUState.Caption:='~' + Format('%.1f', [Swr])
    end else
      PanelATUState.Caption:=Format('%.1f', [Swr]);

    if not Spert.isAtuActive then
      if (Swr > 5) then PanelATUState.Color:=clRed else PanelATUState.Color:=clGray;

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

    PanelATUState.Color:=clGray;
    PanelATUColor.Color:=clGray;

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

  // save current TX state for next loop
  txWas:=Spert.txActive;

  EndFormUpdate;
end;

procedure TFormSpert.TimerCleanupTimer(Sender: TObject);
begin
  PanelPavg.Color:=clGray;
  PanelPavg.Caption:='0';

  PanelPref.Color:=clGray;
  PanelPref.Font.Color:=clWhite;
  PanelPref.Caption:='0.0';

  PanelPmax.Color:=clGray;
  PanelPmax.Caption:='0';

  PanelPcur.Color:=clGray;
  PanelPcur.Caption:='0';

  TimerCleanup.Enabled:=False;
end;


procedure TFormSpert.setUpDatabase;
begin
  databaseTransaction.StartTransaction;
  databaseConnection.ExecuteDirect('CREATE TABLE IF NOT EXISTS "swr_samples" (antenna TEXT NOT NULL, frequency INTEGER NOT NULL, swr REAL, ts INTEGER)');
  databaseConnection.ExecuteDirect('CREATE INDEX IF NOT EXISTS "swr_samples_antenna" ON  "swr_samples" (antenna, frequency)');
  databaseTransaction.Commit;
end;

procedure TFormSpert.insertSwr;
var
  Frq: Longword;
  Sql: String;
  rCount: Integer;
begin
  Frq:=Trunc(Rig.getFrq / 1000) * 1000;
  databaseTransaction.StartTransaction;
  databaseQuery.SQL.Text:='SELECT COUNT(*) FROM swr_samples WHERE antenna = "' + Configuration.Settings.spertAntenna + '" AND frequency = ' + IntToStr(Frq);
  databaseQuery.Open;
  rCount:=databaseQuery.Fields[0].AsInteger;
  databaseQuery.Close;
  if rCount = 0
    then Sql:='INSERT INTO swr_samples (antenna, frequency, swr, ts) VALUES("' + Configuration.Settings.spertAntenna + '",' + IntToStr(Frq)  + ',' + FloatToStr(Spert.getRpwrAvgPercent) + ',' + IntToStr(DateTimeToUnix(Now())) + ')'
    else Sql:='UPDATE swr_samples SET swr = ' + FloatToStr(Spert.getRpwrAvgPercent)  + ', ts = ' + IntToStr(DateTimeToUnix(Now())) + ' WHERE antenna = "' + Configuration.Settings.spertAntenna + '" AND frequency = ' + IntToStr(Frq);
  FormDebug.Log('[Spert] ' + Sql);
  databaseConnection.ExecuteDirect(sql);
  databaseTransaction.Commit;
end;

function TFormSpert.estimateSwr(span: Integer): Double;
var
  Frq: Longword;
  Swr: String;
begin
  Frq:=Trunc(Rig.getFrq / 1000) * 1000;
  databaseTransaction.StartTransaction;
  databaseQuery.SQL.Text:='SELECT AVG(swr) FROM swr_samples WHERE antenna = "' + Configuration.Settings.spertAntenna + '" AND frequency BETWEEN ' + IntToStr(Frq - span) + ' AND ' + IntToStr(Frq + span);
  databaseQuery.Open;
  Swr:=databaseQuery.Fields[0].AsString;
  databaseQuery.Close;
  if Swr <> '' then Result:=StrToFloat(Swr) else Result:=-1;
  databaseTransaction.Commit;
end;

procedure TFormSpert.ReloadConfiguration(restart: Boolean);
begin
  Configuration.Load;
  if restart then
  begin
    Spert.Stop;
    Spert.Start;
  end;
end;

end.
