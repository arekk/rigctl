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
    procedure ConfigurationFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
  private

  public

  end;

var
  FormInit: TFormInit;

implementation

{$R *.lfm}

{ TFormInit }

procedure TFormInit.MenuItem2Click(Sender: TObject);
begin
  Form3.OnClose:=@ConfigurationFormClose;
  Form3.Show;
end;

procedure TFormInit.ConfigurationFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Configuration.Settings.Debug and not FormDebug.Visible
     then FormDebug.Show;

  if FormDebug.Visible and not Configuration.Settings.Debug
     then FormDebug.Close;

  FormSpert.ReloadConfiguration;
  FormRig.ReloadConfiguration;

  Form3.OnClose:=nil;
end;

procedure TFormInit.MenuItem5Click(Sender: TObject);
begin
  FormRig.Show;
end;

procedure TFormInit.MenuItem6Click(Sender: TObject);
begin
  FormSpert.Show;
end;

end.

