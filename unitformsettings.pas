unit UnitFormSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls, Spin,
  UnitSettings;

type

  { TForm3 }

  TForm3 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form3: TForm3;
  Configuration: TConfiguration;

implementation

{$R *.lfm}

{ TForm3 }

procedure TForm3.FormShow(Sender: TObject);
var
  searchResult: TSearchRec;
begin
  Configuration:=TConfiguration.Create(Application.Location);
  Configuration.Load;

  CheckBox1.Checked:=Configuration.Settings.Debug;
  ComboBox1.ItemIndex:=Configuration.Settings.spertStartupFan;

  ComboBox2.Items.Clear;
  ComboBox2.Items.Add('AUTO');

  ComboBox3.Items.Clear;
  ComboBox3.Items.Add('AUTO');

  if FindFirst('/dev/tty.*', faAnyFile, searchResult) = 0 then
  begin
    repeat begin
      ComboBox2.Items.Add('/dev/' + searchResult.Name);
      ComboBox3.Items.Add('/dev/' + searchResult.Name);
    end until FindNext(searchResult) <> 0;

    FindClose(searchResult);
  end;

  if ComboBox2.Items.IndexOf(Configuration.Settings.spertPort) >= 0
    then ComboBox2.ItemIndex:=ComboBox2.Items.IndexOf(Configuration.Settings.spertPort);

  if ComboBox3.Items.IndexOf(Configuration.Settings.trxPort) >= 0
    then ComboBox3.ItemIndex:=ComboBox3.Items.IndexOf(Configuration.Settings.trxPort);

  CheckBox2.Checked:=Configuration.Settings.flrigServerEnabled;
  SpinEdit1.Value:=Configuration.Settings.flrigServerPort;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  Configuration.Settings.Debug:=CheckBox1.Checked;
  Configuration.Settings.spertStartupFan:=ComboBox1.ItemIndex;
  if ComboBox2.ItemIndex >= 0
    then Configuration.Settings.spertPort:=ComboBox2.Items[ComboBox2.ItemIndex];
  if ComboBox3.ItemIndex >= 0
    then Configuration.Settings.trxPort:=ComboBox3.Items[ComboBox3.ItemIndex];
  Configuration.Settings.flrigServerEnabled:=CheckBox2.Checked;
  Configuration.Settings.flrigServerPort:=SpinEdit1.Value;
  Configuration.Save;
  Close;
end;

procedure TForm3.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
end;

end.

