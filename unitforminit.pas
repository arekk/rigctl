unit UnitFormInit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  Menus,
  ExtCtrls,
  UnitSettings,
  UnitSerialPortAutoDiscover,
  UnitEventsInterface,
  UnitEvents,
  UnitSpert,
  UnitFormSettings,
  UnitFormDebug,
  UnitFormSpert,
  UnitFormRig;

type
  { TFormInit }

  TFormInit = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Separator1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure ConfigurationFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
    Configuration: TConfiguration;
  private
    procedure SetUp;
  end;

var
  FormInit: TFormInit;

implementation

{$R *.lfm}

{ TFormInit }

procedure TFormInit.FormCreate(Sender: TObject);
var
  autoDiscover: TSerialPortDiscover;
begin
  Width:=0;
  Height:=0;

  Configuration:=TConfiguration.Create(Application.Location);
  Configuration.Load;

  autoDiscover:=TSerialPortDiscover.Create(configuration.getConfigDirectory);
  autoDiscover.CleanLockFiles;
end;

procedure TFormInit.FormShow(Sender: TObject);
var
  hwEvent: IEvent;
begin
  hwEvent:=TEvent.Create(FormSpert.Spert, FormSpert.SpertDatabase, FormRig.Rig, Configuration);

  FormSpert.Spert.onEvent(hwEvent);
  FormRig.Rig.onEvent(hwEvent);

  SetUp;
end;

procedure TFormInit.MenuItem2Click(Sender: TObject);
begin
  FormSettings.OnClose:=@ConfigurationFormClose;
  FormSettings.Show;
end;

procedure TFormInit.ConfigurationFormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  settingsForm: TFormSettings;
begin
  Configuration.Load;

  settingsForm:=TFormSettings(Sender);

  SetUp;
  FormSpert.SetUp(settingsForm.spertReloadRequired);
  FormRig.SetUp(settingsForm.trxReloadRequired, settingsForm.flrigReloadRequired);

  CloseAction:=caHide;
end;

procedure TFormInit.SetUp;
begin
  // window main menu
  MenuItem5.Visible:=Configuration.IsTrxValid;
  MenuItem5.Caption:=Configuration.Settings.trx;

  MenuItem6.Visible:=Configuration.Settings.spertEnabled;
  MenuItem7.Visible:=Configuration.Settings.Debug;

  // settings main menu
  Separator1.Visible:=Configuration.Settings.spertEnabled;
  MenuItem10.Visible:=Configuration.Settings.spertEnabled;
  MenuItem11.Visible:=Configuration.Settings.spertEnabled;

  // windows
  if Configuration.Settings.Debug and not FormDebug.Visible
    then FormDebug.Show;
  if FormDebug.Visible and not Configuration.Settings.Debug
    then FormDebug.Close;

  if FormRig.Visible and not Configuration.IsTrxValid
    then FormRig.Close;
  if not FormRig.Visible and Configuration.IsTrxValid
    then FormRig.Show;

  if FormSpert.Visible and not Configuration.Settings.spertEnabled
    then FormSpert.Close;
  if not FormSpert.Visible and Configuration.Settings.spertEnabled
    then FormSpert.Show;
end;

procedure TFormInit.MenuItem5Click(Sender: TObject);
begin
  FormRig.Show;
end;

procedure TFormInit.MenuItem6Click(Sender: TObject);
begin
  FormSpert.Show;
end;

procedure TFormInit.MenuItem7Click(Sender: TObject);
begin
  FormDebug.Show;
end;

procedure TFormInit.MenuItem8Click(Sender: TObject);
var
  helpKeboard: TStringList;
begin
  helpKeboard:=TStringList.Create;
  helpKeboard.Delimiter:=#13;

  if Configuration.IsTrxValid then
  begin
    helpKeboard.Add('TRX');
    helpKeboard.Add('');
    helpKeboard.Add('VOX on/off.................... ⌘+backspace');
    helpKeboard.Add('Split on/off.................. ⌘+S');
    helpKeboard.Add('Split +5...................... ⌘+P');
    helpKeboard.Add('Split -5...................... ⌘+L');
    helpKeboard.Add('TXW........................... ⌘+Enter');
    helpKeboard.Add('Power +1W..................... ⌘+Right arrow');
    helpKeboard.Add('Power -1W..................... ⌘+Left arrow');
    helpKeboard.Add('Power 10,20,30................ ⌘+1,2,3');
    helpKeboard.Add('DGain +5...................... ⌘+Up arrow');
    helpKeboard.Add('DGain -5...................... ⌘+Down arrow');
    helpKeboard.Add('');
  end;
  if Configuration.Settings.spertEnabled then
  begin
    helpKeboard.Add('SPert');
    helpKeboard.Add('');
    helpKeboard.Add('ATU on/off.................... ⌘+A');
    helpKeboard.Add('Run ATU tuning................ ⌘+T');
    helpKeboard.Add('Power 100,200,300............. ⌘+1,2,3');
  end;
  MessageDlg('Keyboard shortcuts', helpKeboard.GetText, mtCustom, [mbOK], 0);
end;

procedure TFormInit.MenuItem12Click(Sender: TObject);
begin
  FormSpert.Spert.setFan(SpertFanLevel_Max);
end;

procedure TFormInit.MenuItem11Click(Sender: TObject);
begin
  FormSpert.Spert.resetAtuMem;
  FormSpert.SpertDatabase.Clear;
end;

procedure TFormInit.MenuItem13Click(Sender: TObject);
begin
  FormSpert.Spert.setFan(SpertFanLevel_Histeresis);
end;

procedure TFormInit.MenuItem14Click(Sender: TObject);
begin
  FormSpert.Spert.setFan(SpertFanLevel_Fixed);
end;

procedure TFormInit.MenuItem15Click(Sender: TObject);
var
  helpAbout: TStringList;
begin
  helpAbout:=TStringList.Create;
  helpAbout.Delimiter:=#13;
  helpAbout.Add('Author');
  helpAbout.Add('Sugar Mice Software, Arkadiusz Kuryłowicz');
  helpAbout.Add('SO9W');
  helpAbout.Add('https://sugarmice.software/');
  MessageDlg('Keyboard shortcuts', helpAbout.GetText, mtCustom, [mbOK], 0);
end;

procedure TFormInit.MenuItem9Click(Sender: TObject);
begin
  FormSpert.Spert.setFan(SpertFanLevel_50);
end;

end.

