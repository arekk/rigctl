unit UnitRigFT991A;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  UnitSettings,
  UnitRig,
  UnitSerialPortAutoDiscover,
  UnitRigFTdx10;

type
  TFT991Arig = class(TFTdx10rig)
    public
      procedure SetSplit(active: Boolean); override;
      procedure SetdrPortGain(gain: Byte); override;
    protected
      function getSerialPortDiscoverParams: TSerialPortDiscoverParams; override;
  end;

implementation

function TFT991Arig.getSerialPortDiscoverParams: TSerialPortDiscoverParams;
begin
  Result:=TSerialPortDiscoverParams.Create(TSettingsTrx.FT991A, Configuration.Settings.trxPortRate, Utf8ToAnsi(';'), Utf8ToAnsi('ID'), Utf8ToAnsi('ID0670'), 50)
end;

procedure TFT991Arig.SetSplit(active: Boolean);
begin
  // nothing - it's not supported
end;

procedure TFT991Arig.SetdrPortGain(gain: Byte);
begin
  // nothing - it's not supported
end;

end.

