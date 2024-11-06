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
    procedure ConfigurationFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
  private
    Configuration: TConfiguration;
  end;

var
  FormInit: TFormInit;

implementation

{$R *.lfm}

{ TFormInit }

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
  if Configuration.Settings.Debug and not FormDebug.Visible
     then FormDebug.Show;

  if FormDebug.Visible and not Configuration.Settings.Debug
     then FormDebug.Close;

  FormSpert.ReloadConfiguration;
  FormRig.ReloadConfiguration;

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

end.

