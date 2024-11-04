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

    {
    flrigEnabled: Boolean;
    flrigRpcHostname: String;
    flrigRpcPort: Integer;
    }

    TrxTunePower: Integer;
    AtuOffOnBandChange: Boolean;

    macroModeA: Boolean;
    macroModeAMod: String;
    macroModeAPwr: Integer;
    macroModeAFan: Integer;
    macroModeB: Boolean;
    macroModeBMod: String;
    macroModeBPwr: Integer;
    macroModeBFan: Integer;
    macroModeC: Boolean;
    macroModeCMod: String;
    macroModeCPwr: Integer;
    macroModeCFan: Integer;
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
begin
  iniPath:=ExtractFilePath(applicationLocation) + 'rig.ini';
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
    Settings.spertPort:=iniFile.ReadString('SPert', 'Port', '');
    Settings.spertPortRate:=iniFile.ReadInteger('SPert', 'PortRate', 9600);
    Settings.spertPool:=iniFile.ReadInteger('SPert', 'Pool', 200);
    Settings.spertStartupFan:=iniFile.ReadInteger('SPert', 'DefaultFan', 0);

    Settings.trxEnabled:=iniFile.ReadBool('FTdx10', 'Enabled', true);
    Settings.trxPort:=iniFile.ReadString('FTdx10', 'Port', '');
    Settings.trxPortRate:=iniFile.ReadInteger('FTdx10', 'PortRate', 38400);
    Settings.trxPool:=iniFile.ReadInteger('FTdx10', 'Pool', 200);

    Settings.flrigServerEnabled:=iniFile.ReadBool('FlrigServer', 'Enabled', false);
    Settings.flrigServerPort:=iniFile.ReadInteger('FlrigServer', 'Port', 12345);


    Settings.TrxTunePower:=iniFile.ReadInteger('Macro', 'TrxTunePower', 0);
    Settings.AtuOffOnBandChange:=iniFile.ReadBool('Macro', 'AtuOffOnBandChange', false);

    Settings.macroModeA:=iniFile.ReadBool('Macro', 'ModeA', false);
    Settings.macroModeAMod:=iniFile.ReadString('Macro', 'ModeAMod', '');
    Settings.macroModeAPwr:=iniFile.ReadInteger('Macro', 'ModeAPwr', 0);
    Settings.macroModeAFan:=iniFile.ReadInteger('Macro', 'ModeAFan', 0);

    Settings.macroModeB:=iniFile.ReadBool('Macro', 'ModeB', false);
    Settings.macroModeBMod:=iniFile.ReadString('Macro', 'ModeBMod', '');
    Settings.macroModeBPwr:=iniFile.ReadInteger('Macro', 'ModeBPwr', 0);
    Settings.macroModeBFan:=iniFile.ReadInteger('Macro', 'ModeBFan', 0);

    Settings.macroModeC:=iniFile.ReadBool('Macro', 'ModeC', false);
    Settings.macroModeCMod:=iniFile.ReadString('Macro', 'ModeCMod', '');
    Settings.macroModeCPwr:=iniFile.ReadInteger('Macro', 'ModeCPwr', 0);
    Settings.macroModeCFan:=iniFile.ReadInteger('Macro', 'ModeCFan', 0);
  finally
    iniFile.Free;
  end;
end;

procedure TConfiguration.Save;
var
  iniFile: TIniFile;
begin
  try
    iniMutex.Acquire;;

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
    iniFile.WriteString('Macro', 'ModeAMod', Settings.macroModeAMod);
    iniFile.WriteInteger('Macro', 'ModeAPwr', Settings.macroModeAPwr);
    iniFile.WriteInteger('Macro', 'ModeAFan', Settings.macroModeAFan);

    iniFile.WriteBool('Macro', 'ModeB', Settings.macroModeB);
    iniFile.WriteString('Macro', 'ModeBMod', Settings.macroModeBMod);
    iniFile.WriteInteger('Macro', 'ModeBPwr', Settings.macroModeBPwr);
    iniFile.WriteInteger('Macro', 'ModeBFan', Settings.macroModeBFan);

    iniFile.WriteBool('Macro', 'ModeC', Settings.macroModeC);
    iniFile.WriteString('Macro', 'ModeCMod', Settings.macroModeCMod);
    iniFile.WriteInteger('Macro', 'ModeCPwr', Settings.macroModeCPwr);
    iniFile.WriteInteger('Macro', 'ModeCFan', Settings.macroModeCFan);
  finally
    iniMutex.Release;
    iniFile.Free;
  end;
end;

end.

