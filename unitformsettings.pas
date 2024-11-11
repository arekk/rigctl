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

  { TFormSettings }

  TFormSettings = class(TForm)
    ButtonSave: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormSettings: TFormSettings;
  Configuration: TConfiguration;

implementation

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.FormShow(Sender: TObject);
var
  searchResult: TSearchRec;
begin
  Configuration:=TConfiguration.Create(Application.Location);
  Configuration.Load;

  CheckBox1.Checked:=Configuration.Settings.Debug;

  // SPERT
  CheckBox3.Checked:=Configuration.Settings.spertEnabled;
  ComboBox1.ItemIndex:=Configuration.Settings.spertStartupFan;

  // RIG
  CheckBox4.Checked:=Configuration.Settings.trxEnabled;
  CheckBox2.Checked:=Configuration.Settings.flrigServerEnabled;
  SpinEdit1.Value:=Configuration.Settings.flrigServerPort;

  // Serial ports
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

  // MACROS
  CheckBox5.Checked:=Configuration.Settings.AtuOffOnBandChange;
  SpinEdit2.Value:=Configuration.Settings.TrxTunePower;

end;

procedure TFormSettings.ButtonSaveClick(Sender: TObject);
begin
  Configuration.Settings.Debug:=CheckBox1.Checked;

  // SPERT
  Configuration.Settings.spertEnabled:=CheckBox3.Checked;
  Configuration.Settings.spertStartupFan:=ComboBox1.ItemIndex;
  if ComboBox2.ItemIndex >= 0
    then Configuration.Settings.spertPort:=ComboBox2.Items[ComboBox2.ItemIndex];

  // RIG
  Configuration.Settings.trxEnabled:=CheckBox4.Checked;
  if ComboBox3.ItemIndex >= 0
    then Configuration.Settings.trxPort:=ComboBox3.Items[ComboBox3.ItemIndex];
  Configuration.Settings.flrigServerEnabled:=CheckBox2.Checked;
  Configuration.Settings.flrigServerPort:=SpinEdit1.Value;

  // MACROS
  Configuration.Settings.AtuOffOnBandChange:=CheckBox5.Checked;
  Configuration.Settings.TrxTunePower:=SpinEdit2.Value;

  // save configuration
  Configuration.Save;
  Close;
end;

procedure TFormSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
end;

end.

