unit UnitSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Syncobjs,
  IniFiles;

type

  TSettingsTrx = class
    const FTDX10 = 'FTdx10';
    const FT991A = 'FT-991A';
  end;

  TSettings = record
    debug: Boolean;

    spertEnabled: Boolean;
    spertStartupFan: Integer;
    spertPort: String;
    spertPortRate: Integer;
    spertPool: Integer;
    spertAntenna: String;

    trx: String;
    trxPort: String;
    trxPortRate: Integer;
    trxPool: Integer;

    flrigServerEnabled: Boolean;
    flrigServerPort: Integer;

    spertLockOnSwr: Integer;
    trxTunePower: Integer;
    atuOffOnBandChange: Boolean;

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
      function getConfigDirectory: String;
    public
      Settings: TSettings;
    private
      iniMutex: TCriticalSection;
      configDirectory: String;
      iniPath: String;
  end;

implementation

constructor TConfiguration.Create(applicationLocation: String);
begin
  {$IFDEF DARWIN}
  configDirectory:=GetUserDir + PathDelim + '.config' + PathDelim + 'radioshackctl';
  if (not DirectoryExists(configDirectory)) then CreateDir(configDirectory);
  {$ELSE}
  configDirectory:=ExtractFilePath(applicationLocation);
  {$ENDIF}

  iniPath:=configDirectory + PathDelim + 'radioshackctl.ini';

  iniMutex:=TCriticalSection.Create;
end;

function TConfiguration.getConfigDirectory:String;
begin
  Result:=configDirectory;
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
    Settings.spertAntenna:=iniFile.ReadString('SPert', 'Antenna', 'Default');

    Settings.trx:=Trim(iniFile.ReadString('Trx', 'Model', ''));
    Settings.trxPort:=iniFile.ReadString('Trx', 'Port', 'AUTO');
    Settings.trxPortRate:=iniFile.ReadInteger('Trx', 'PortRate', 38400);
    Settings.trxPool:=iniFile.ReadInteger('Trx', 'Pool', 200);

    Settings.flrigServerEnabled:=iniFile.ReadBool('FlrigServer', 'Enabled', false);
    Settings.flrigServerPort:=iniFile.ReadInteger('FlrigServer', 'Port', 12345);

    Settings.trxTunePower:=iniFile.ReadInteger('Macro', 'TrxTunePower', 0);
    Settings.atuOffOnBandChange:=iniFile.ReadBool('Macro', 'AtuOffOnBandChange', false);
    Settings.spertLockOnSwr:=iniFile.ReadInteger('Macro', 'SpertLockOnSwr', 0);

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
    iniFile.WriteString('SPert', 'Antenna', Settings.spertAntenna);

    iniFile.WriteString('Trx', 'Model', Settings.trx);
    iniFile.WriteString('Trx', 'Port', Settings.trxPort);
    iniFile.WriteInteger('Trx', 'PortRate', Settings.trxPortRate);
    iniFile.WriteInteger('Trx', 'Pool', Settings.trxPool);

    iniFile.WriteBool('FlrigServer', 'Enabled', Settings.flrigServerEnabled);
    iniFIle.WriteInteger('FlrigServer', 'Port', Settings.flrigServerPort);

    iniFile.WriteInteger('Macro', 'TrxTunePower', Settings.trxTunePower);
    iniFile.WriteBool('Macro', 'AtuOffOnBandChange', Settings.atuOffOnBandChange);
    iniFile.WriteInteger('Macro', 'SpertLockOnSwr', Settings.spertLockOnSwr);


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
