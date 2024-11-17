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
      constructor Create(sp: TSpert; r: TRig; c: TConfiguration);
      procedure call(event: TSpertEventType);
    private
      Spert: TSpert;
      Rig: TRig;
      Configuration: TConfiguration;
      trxTunePower: Byte;
      trxPowerSavePoint: Byte;
  end;

  { events on rig that should do somethong on SPert }
  TSpertRigEvent = class(TInterfacedObject, TRigEvent)
    public
      constructor Create(sp: TSpert; r: TRig; c: TConfiguration);
      procedure call(event: TRigEventType);
    private
      Spert: TSpert;
      Rig: TRig;
      Configuration: TConfiguration;
  end;

implementation

constructor TSpertSpertEvent.Create(sp: TSpert; r: TRig; c: TConfiguration);
begin
  Spert:=sp;
  Rig:=r;
  Configuration:=c;

  trxTunePower:=Configuration.Settings.TrxTunePower;
  trxPowerSavePoint:=0;
end;

procedure TSpertSpertEvent.call(event: TSpertEventType);
var
  enumVal: String;
begin
  WriteStr(enumVal, event);
  FormDebug.Log('[Spert] event ' + enumVal);

  if Assigned(Rig) and Rig.isActive then
  begin
    case event of
      SpertEventBeforeTuneStart: begin
        { ATU tunning can set TRX power to 5W }
        if (trxTunePower > 0) and (trxPowerSavePoint = 0) and (not Rig.pttActive) then
        begin
          FormDebug.Log('[Spert] setting trx power to: ' + IntToStr(trxTunePower));
          trxPowerSavePoint:=Rig.getPwr;
          Rig.SetPwr(trxTunePower);
        end;
      end;

      SpertEventAtferTuneStop: begin
        { restore trx power if atu stopped tuning }
        if (trxPowerSavePoint > 0) and not Spert.txActive and not Spert.isAtuTunning then
        begin
          FormDebug.Log('[Spert] restoring trx power: ' + IntToStr(trxPowerSavePoint));
          Rig.SetPwr(trxPowerSavePoint);
        end;
        trxPowerSavePoint:=0;
      end;
    end;
  end;
end;

constructor TSpertRigEvent.Create(sp: TSpert; r: TRig; c: TConfiguration);
begin
  Spert:=sp;
  Rig:=r;
  Configuration:=c;
end;

procedure TSpertRigEvent.call(event: TRigEventType);
var
  enumVal: String;
begin
  WriteStr(enumVal, event);
  FormDebug.Log('[Rig] event ' + enumVal);

  if Assigned(Spert) and Spert.isActive then begin
    case event of
      RigEventBandChange: begin
        if Configuration.Settings.AtuOffOnBandChange and Spert.isAtuActive then Spert.toggleAtuState;
      end;

      RigEventModeChange: begin
        if Configuration.Settings.macroModeA and ContainsText(Configuration.Settings.macroModeAMod, Rig.getMode) and (Configuration.Settings.macroModeASpertPwr > 0)
          then Spert.setPower(Round(Configuration.Settings.macroModeASpertPwr / 10));

        if Configuration.Settings.macroModeB and ContainsText(Configuration.Settings.macroModeBMod, Rig.getMode) and (Configuration.Settings.macroModeBSpertPwr > 0)
          then Spert.setPower(Round(Configuration.Settings.macroModeBSpertPwr / 10));
      end;
    end;
  end;

  if Assigned(Rig) and Rig.isActive then begin
    case event of
      RigEventModeChange: begin
        if Configuration.Settings.macroModeA and ContainsText(Configuration.Settings.macroModeAMod, Rig.getMode) and (Configuration.Settings.macroModeATrxPwr > 0)
          then Rig.SetPwr(Configuration.Settings.macroModeATrxPwr);

        if Configuration.Settings.macroModeB and ContainsText(Configuration.Settings.macroModeBMod, Rig.getMode) and (Configuration.Settings.macroModeBTrxPwr > 0)
          then Rig.SetPwr(Configuration.Settings.macroModeBTrxPwr);
      end;
    end;
  end;
end;

end.

