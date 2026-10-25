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
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
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
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
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
  ComboBox4.Items.Add(TSettingsTrx.FT991A);
  if ComboBox4.Items.IndexOf(Configuration.Settings.trx) >= 0
    then ComboBox4.ItemIndex:=ComboBox4.Items.IndexOf(Configuration.Settings.trx);

  ComboBox5.Items.Clear;
  ComboBox5.Items.Add('4800');
  ComboBox5.Items.Add('9600');
  ComboBox5.Items.Add('19200');
  ComboBox5.Items.Add('38400');
  if ComboBox5.Items.IndexOf(IntToStr(Configuration.Settings.trxPortRate)) >= 0
    then ComboBox5.ItemIndex:=ComboBox5.Items.IndexOf(IntToStr(Configuration.Settings.trxPortRate));

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

  { Mode-category dropdowns are populated from the LFM's Items.Strings.
    IndexOf returns -1 if the stored value isn't one of PHONO/DATA/CW (e.g. legacy
    free-text "usb" from before this change) — in that case ItemIndex stays at -1
    (blank selection) and the user must pick a category to make the macro fire. }
  CheckBox4.Checked:=Configuration.Settings.macroModeA;
  ComboBox6.ItemIndex:=ComboBox6.Items.IndexOf(Configuration.Settings.macroModeAMod);
  SpinEdit4.Value:=Configuration.Settings.macroModeATrxPwr;
  SpinEdit5.Value:=Configuration.Settings.macroModeASpertPwr;

  CheckBox6.Checked:=Configuration.Settings.macroModeB;
  ComboBox7.ItemIndex:=ComboBox7.Items.IndexOf(Configuration.Settings.macroModeBMod);
  SpinEdit6.Value:=Configuration.Settings.macroModeBTrxPwr;
  SpinEdit7.Value:=Configuration.Settings.macroModeBSpertPwr;
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
  if ((ComboBox4.ItemIndex >= 0) and (Configuration.Settings.trx <> ComboBox4.Items[ComboBox4.ItemIndex])) or ((ComboBox3.ItemIndex >= 0) and (Configuration.Settings.trxPort <> ComboBox3.Items[ComboBox3.ItemIndex])) or ((ComboBox5.ItemIndex >= 0) and (Configuration.Settings.trxPortRate <> StrToInt(ComboBox5.Items[ComboBox5.ItemIndex])))
    then trxReloadRequired:=True;

  if ComboBox4.ItemIndex >= 0
    then Configuration.Settings.trx:=ComboBox4.Items[ComboBox4.ItemIndex];
  if ComboBox5.ItemIndex >= 0
    then Configuration.Settings.trxPortRate:=StrToInt(ComboBox5.Items[ComboBox5.ItemIndex]);
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

  Configuration.Settings.macroModeA:=CheckBox4.Checked;
  if ComboBox6.ItemIndex >= 0
    then Configuration.Settings.macroModeAMod:=ComboBox6.Items[ComboBox6.ItemIndex]
    else Configuration.Settings.macroModeAMod:='';
  Configuration.Settings.macroModeATrxPwr:=SpinEdit4.Value;
  Configuration.Settings.macroModeASpertPwr:=SpinEdit5.Value;

  Configuration.Settings.macroModeB:=CheckBox6.Checked;
  if ComboBox7.ItemIndex >= 0
    then Configuration.Settings.macroModeBMod:=ComboBox7.Items[ComboBox7.ItemIndex]
    else Configuration.Settings.macroModeBMod:='';
  Configuration.Settings.macroModeBTrxPwr:=SpinEdit6.Value;
  Configuration.Settings.macroModeBSpertPwr:=SpinEdit7.Value;

  // save configuration
  Configuration.Save;
  Close;
end;

end.

