unit UnitEvents;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,

  UnitSettings,
  UnitSpert,
  UnitRig,

  UnitFormDebug;

type
  
  { events on SPert that should do someething on rig }
  TSpertSpertEvent = class(TInterfacedObject, TSpertEvent)
    public
      constructor Create(r: TRig; s: TSettings);
      procedure call(event: Byte);

    private
      Rig: TRig;
      Settings: TSettings;
      trxTunePower: Byte;
      trxPowerSavePoint: Byte;
  end;

  { events on rig that should do somethong on SPert }
  TSpertRigEvent = class(TInterfacedObject, TRigEvent)
    public
      constructor Create(sp: TSpert; r: TRig; se: TSettings);
      procedure call(event: Byte);
    private
      Spert: TSpert;
      Rig: TRig;
      Settings: TSettings;
  end;

implementation

constructor TSpertSpertEvent.Create(r: TRig; s: TSettings);
begin
  Rig:=r;
  Settings:=s;
  trxTunePower:=Settings.TrxTunePower;
  trxPowerSavePoint:=0;
end;

procedure TSpertSpertEvent.call(event: Byte);
begin
  FormDebug.Log('[Spert] event: ' + IntToStr(event));

  if Assigned(Rig) and Rig.isActive then
  begin
    case event of
      TSpertEvents.BEFORE_TUNE_START: begin
        { ATU tunning can set TRX power to 5W }
        if (trxTunePower > 0) and (trxPowerSavePoint = 0) and (not Rig.pttActive) then
        begin
          FormDebug.Log('[Spert] setting trx power to: ' + IntToStr(trxTunePower));
          trxPowerSavePoint:=Rig.getPwr;
          Rig.SetPwr(trxTunePower);
        end;
      end;

      TSpertEvents.AFTER_TUNE_END: begin
        { restore trx power if atu stopped tuning }
        if (trxPowerSavePoint > 0) then
        begin
          FormDebug.Log('[Spert] restoring trx power: ' + IntToStr(trxPowerSavePoint));
          Rig.SetPwr(trxPowerSavePoint);
        end;
        trxPowerSavePoint:=0;
      end;
    end;
  end;
end;

constructor TSpertRigEvent.Create(sp: TSpert; r: TRig; se: TSettings);
begin
  Spert:=sp;
  Rig:=r;
  Settings:=se;
end;

procedure TSpertRigEvent.call(event: Byte);
begin
  FormDebug.Log('[Rig] event: ' + IntToStr(event));

  if Assigned(Spert) and Spert.isActive then begin
    case event of
      TRigEvents.BAND_CHANGE: begin
        if Settings.AtuOffOnBandChange and Spert.isAtuActive then Spert.toggleAtuState;
      end;

      TRigEvents.MODE_CHANGE: begin
        if Settings.macroModeA and ContainsText(Settings.macroModeAMod, Rig.getMode) and (Settings.macroModeASpertPwr > 0)
          then Spert.setPower(Round(Settings.macroModeASpertPwr / 10));

        if Settings.macroModeB and ContainsText(Settings.macroModeBMod, Rig.getMode) and (Settings.macroModeBSpertPwr > 0)
          then Spert.setPower(Round(Settings.macroModeBSpertPwr / 10));
      end;
    end;
  end;

  if Assigned(Rig) and Rig.isActive then begin
    case event of
      TRigEvents.BAND_CHANGE: begin

      end;

      TRigEvents.MODE_CHANGE: begin
        if Settings.macroModeA and ContainsText(Settings.macroModeAMod, Rig.getMode) and (Settings.macroModeATrxPwr > 0)
          then Rig.SetPwr(Settings.macroModeATrxPwr);

        if Settings.macroModeB and ContainsText(Settings.macroModeBMod, Rig.getMode) and (Settings.macroModeBTrxPwr > 0)
          then Rig.SetPwr(Settings.macroModeBTrxPwr);
      end;
    end;
  end;
end;

end.

