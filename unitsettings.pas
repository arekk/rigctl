unit UnitSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Syncobjs,
  IniFiles;

type

  TSettings = record
    debug: Boolean;

    spertEnabled: Boolean;
    spertStartupFan: Integer;
    spertPort: String;
    spertPortRate: Integer;
    spertPool: Integer;

    trxEnabled: Boolean;
    trxPort: String;
    trxPortRate: Integer;
    trxPool: Integer;

    flrigServerEnabled: Boolean;
    flrigServerPort: Integer;

    TrxTunePower: Integer;
    AtuOffOnBandChange: Boolean;

    macroModeA: Boolean;
    macroModeAMod: String;
    macroModeASpertPwr: Integer;
    macroModeATrxPwr: Integer;

    macroModeB: Boolean;
    macroModeBMod: String;
    macroModeBSpertPwr: Integer;
    macroModeBTrxPwr: Integer;
  end;

  TConfiguration = class
    procedure Load;
    procedure Save;

    public
      constructor Create(applicationLocation: String);
    private
      iniMutex: TCriticalSection;
    public
      iniPath: String;
      Settings: TSettings;
  end;

implementation

constructor TConfiguration.Create(applicationLocation: String);
var iniDir: String;
begin
  {$IFDEF DARWIN}
  iniDir:=GetUserDir + PathDelim + '.config' + PathDelim + 'radioshackctl';
  if (not DirectoryExists(iniDir)) then CreateDir(iniDir);
  {$ELSE}
  iniDir:=ExtractFilePath(applicationLocation);
  {$ENDIF}

  iniPath:=iniDir + PathDelim + 'radioshackctl.ini';

  iniMutex:=TCriticalSection.Create;
end;

procedure TConfiguration.Load;
var
  iniFile: TIniFile;
begin
  try
    iniFile:=TIniFile.Create(iniPath);

    Settings.Debug:=iniFile.ReadBool('Generic', 'Debug', false);

    Settings.spertEnabled:=iniFile.ReadBool('SPert', 'Enabled', true);
    Settings.spertPort:=iniFile.ReadString('SPert', 'Port', 'AUTO');
    Settings.spertPortRate:=iniFile.ReadInteger('SPert', 'PortRate', 9600);
    Settings.spertPool:=iniFile.ReadInteger('SPert', 'Pool', 200);
    Settings.spertStartupFan:=iniFile.ReadInteger('SPert', 'DefaultFan', 0);

    Settings.trxEnabled:=iniFile.ReadBool('FTdx10', 'Enabled', true);
    Settings.trxPort:=iniFile.ReadString('FTdx10', 'Port', 'AUTO');
    Settings.trxPortRate:=iniFile.ReadInteger('FTdx10', 'PortRate', 38400);
    Settings.trxPool:=iniFile.ReadInteger('FTdx10', 'Pool', 200);

    Settings.flrigServerEnabled:=iniFile.ReadBool('FlrigServer', 'Enabled', false);
    Settings.flrigServerPort:=iniFile.ReadInteger('FlrigServer', 'Port', 12345);

    Settings.TrxTunePower:=iniFile.ReadInteger('Macro', 'TrxTunePower', 0);
    Settings.AtuOffOnBandChange:=iniFile.ReadBool('Macro', 'AtuOffOnBandChange', false);

    Settings.macroModeA:=iniFile.ReadBool('Macro', 'ModeA', false);
    Settings.macroModeAMod:=iniFile.ReadString('Macro', 'ModeA_Mod', '');
    Settings.macroModeASpertPwr:=iniFile.ReadInteger('Macro', 'ModeA_SPertPwr', 0);
    Settings.macroModeATrxPwr:=iniFile.ReadInteger('Macro', 'ModeA_TrxPwr', 0);

    Settings.macroModeB:=iniFile.ReadBool('Macro', 'ModeB', false);
    Settings.macroModeBMod:=iniFile.ReadString('Macro', 'ModeB_Mod', '');
    Settings.macroModeBSpertPwr:=iniFile.ReadInteger('Macro', 'ModeB_SPertPwr', 0);
    Settings.macroModeBTrxPwr:=iniFile.ReadInteger('Macro', 'ModeB_TrxPwr', 0);
  finally
    iniFile.Free;
  end;
end;

procedure TConfiguration.Save;
var
  iniFile: TIniFile;
begin
  try
    iniMutex.Acquire;
    iniFile:=TIniFile.Create(iniPath);

    iniFile.WriteBool('Generic', 'Debug', Settings.debug);

    iniFile.WriteBool('SPert', 'Enabled', Settings.spertEnabled);
    iniFile.WriteInteger('SPert', 'StartupFan', Settings.spertStartupFan);
    iniFile.WriteString('SPert', 'Port', Settings.spertPort);
    iniFile.WriteInteger('SPert', 'PortRate', Settings.spertPortRate);
    iniFile.WriteInteger('SPert', 'Pool', Settings.spertPool);

    iniFile.WriteBool('FTdx10', 'Enabled', Settings.trxEnabled);
    iniFile.WriteString('FTdx10', 'Port', Settings.trxPort);
    iniFile.WriteInteger('FTdx10', 'PortRate', Settings.trxPortRate);
    iniFile.WriteInteger('FTdx10', 'Pool', Settings.trxPool);

    iniFile.WriteBool('FlrigServer', 'Enabled', Settings.flrigServerEnabled);
    iniFIle.WriteInteger('FlrigServer', 'Port', Settings.flrigServerPort);

    iniFile.WriteInteger('Macro', 'TrxTunePower', Settings.TrxTunePower);
    iniFile.WriteBool('Macro', 'AtuOffOnBandChange', Settings.AtuOffOnBandChange);

    iniFile.WriteBool('Macro', 'ModeA', Settings.macroModeA);
    iniFile.WriteString('Macro', 'ModeA_Mod', Settings.macroModeAMod);
    iniFile.WriteInteger('Macro', 'ModeA_SPertPwr', Settings.macroModeASpertPwr);
    iniFile.WriteInteger('Macro', 'ModeA_TrxPwr', Settings.macroModeATrxPwr);

    iniFile.WriteBool('Macro', 'ModeB', Settings.macroModeB);
    iniFile.WriteString('Macro', 'ModeB_Mod', Settings.macroModeBMod);
    iniFile.WriteInteger('Macro', 'ModeB_SPertPwr', Settings.macroModeBSpertPwr);
    iniFile.WriteInteger('Macro', 'ModeB_TrxPwr', Settings.macroModeBTrxPwr);
  finally
    iniMutex.Release;
    iniFile.Free;
  end;
end;

end.
