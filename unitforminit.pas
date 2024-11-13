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
  UnitEvents,
  UnitSpert,
  UnitRig,
  UnitFormSettings,
  UnitFormDebug,
  UnitFormSpert,
  UnitFormRig;

type
  { TFormInit }

  TFormInit = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    procedure PostInit;
    procedure ConfigurationFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
    Configuration: TConfiguration;
  end;

var
  FormInit: TFormInit;

implementation

{$R *.lfm}

{ TFormInit }

procedure TFormInit.PostInit;
begin
  FormSpert.Spert.setEventHandler(TSpertSpertEvent.Create(FormRig.Rig, Configuration.Settings));

  FormRig.Rig.setEventHandler(TSpertRigEvent.Create(FormSpert.Spert, FormRig.Rig, Configuration.Settings));
end;

procedure TFormInit.FormCreate(Sender: TObject);
begin
  Configuration:=TConfiguration.Create(Application.Location);
  Configuration.Load;
  Width:=0;
  Height:=0;
  MenuItem7.Visible:=Configuration.Settings.Debug;
end;

procedure TFormInit.MenuItem2Click(Sender: TObject);
begin
  FormSettings.OnClose:=@ConfigurationFormClose;
  FormSettings.Show;
end;

procedure TFormInit.ConfigurationFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Configuration.Load;

  if Configuration.Settings.Debug and not FormDebug.Visible
     then FormDebug.Show;

  if FormDebug.Visible and not Configuration.Settings.Debug
     then FormDebug.Close;

  FormSpert.ReloadConfiguration;
  FormRig.ReloadConfiguration;

  PostInit;

  FormSettings.OnClose:=nil;
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
  helpKeboard.Add('SPert');
  helpKeboard.Add('');
  helpKeboard.Add('ATU on/off.................... ⌘+A');
  helpKeboard.Add('Run ATU tuning................ ⌘+T');
  helpKeboard.Add('Power 100,200,300............. ⌘+1,2,3');

  MessageDlg('Keyboard shortcuts', helpKeboard.GetText, mtCustom, [mbOK], 0);
end;

end.

