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
  ExtCtrls,
  Spin,
  UnitSettings;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    ButtonSave: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox5: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Configuration: TConfiguration;
  public
    trxReloadRequired: Boolean;
    spertReloadRequired: Boolean;
    flrigReloadRequired: Boolean;
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  trxReloadRequired:=False;
  flrigReloadRequired:=False;
  spertReloadRequired:=False;
end;


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
  ComboBox4.Items.Clear;
  ComboBox4.Items.Add('');
  ComboBox4.Items.Add(TSettingsTrx.FTDX10);
  if ComboBox4.Items.IndexOf(Configuration.Settings.trx) >= 0
    then ComboBox4.ItemIndex:=ComboBox4.Items.IndexOf(Configuration.Settings.trx);

  // FLRIG
  CheckBox2.Checked:=Configuration.Settings.flrigServerEnabled;
  SpinEdit1.Value:=Configuration.Settings.flrigServerPort;

  // Serial ports
  ComboBox2.Items.Clear;
  ComboBox2.Items.Add('AUTO');

  ComboBox3.Items.Clear;
  ComboBox3.Items.Add('AUTO');

  {$IFDEF DARWIN}
  if FindFirst('/dev/tty.*', faAnyFile, searchResult) = 0 then
  begin
    repeat begin
      ComboBox2.Items.Add('/dev/' + searchResult.Name);
      ComboBox3.Items.Add('/dev/' + searchResult.Name);
    end until FindNext(searchResult) <> 0;

    FindClose(searchResult);
  end;
  {$ELSE}
  {$ENDIF}

  if ComboBox2.Items.IndexOf(Configuration.Settings.spertPort) >= 0
    then ComboBox2.ItemIndex:=ComboBox2.Items.IndexOf(Configuration.Settings.spertPort);

  if ComboBox3.Items.IndexOf(Configuration.Settings.trxPort) >= 0
    then ComboBox3.ItemIndex:=ComboBox3.Items.IndexOf(Configuration.Settings.trxPort);

  // MACROS
  CheckBox5.Checked:=Configuration.Settings.AtuOffOnBandChange;
  SpinEdit2.Value:=Configuration.Settings.TrxTunePower;
  SpinEdit3.Value:=Configuration.Settings.spertLockOnSwr;
end;

procedure TFormSettings.ButtonSaveClick(Sender: TObject);
begin
  Configuration.Settings.Debug:=CheckBox1.Checked;

  // SPERT
  if (Configuration.Settings.spertEnabled <> CheckBox3.Checked) or ((ComboBox2.ItemIndex >= 0) and (Configuration.Settings.spertPort <> ComboBox2.Items[ComboBox2.ItemIndex]))
    then spertReloadRequired:=True;

  Configuration.Settings.spertEnabled:=CheckBox3.Checked;
  Configuration.Settings.spertStartupFan:=ComboBox1.ItemIndex;
  if ComboBox2.ItemIndex >= 0
    then Configuration.Settings.spertPort:=ComboBox2.Items[ComboBox2.ItemIndex];

  // RIG
  if ((ComboBox4.ItemIndex >= 0) and (Configuration.Settings.trx <> ComboBox4.Items[ComboBox4.ItemIndex])) or ((ComboBox3.ItemIndex >= 0) and (Configuration.Settings.trxPort <> ComboBox3.Items[ComboBox3.ItemIndex]))
    then trxReloadRequired:=True;

  if ComboBox4.ItemIndex >= 0
    then Configuration.Settings.trx:=ComboBox4.Items[ComboBox4.ItemIndex];
  if ComboBox3.ItemIndex >= 0
    then Configuration.Settings.trxPort:=ComboBox3.Items[ComboBox3.ItemIndex];

  // FLRIG
  if (Configuration.Settings.flrigServerEnabled <> CheckBox2.Checked) or (Configuration.Settings.flrigServerPort <> SpinEdit1.Value)
    then flrigReloadRequired:=True;

  Configuration.Settings.flrigServerEnabled:=CheckBox2.Checked;
  Configuration.Settings.flrigServerPort:=SpinEdit1.Value;

  // MACROS
  Configuration.Settings.AtuOffOnBandChange:=CheckBox5.Checked;
  Configuration.Settings.TrxTunePower:=SpinEdit2.Value;
  Configuration.Settings.spertLockOnSwr:=SpinEdit3.Value;

  // save configuration
  Configuration.Save;
  Close;
end;



end.

