unit UnitEvents;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  UnitEventsInterface,
  UnitSettings,
  UnitSpertDatabase,
  UnitSpert,
  UnitRig,
  UnitFormDebug;

type
  TEvent = class(TInterfacedObject, IEvent)
    public
      constructor Create(sp: TSpert; spdb: TSpertDatabase; r: TRig; c: TConfiguration);
      procedure call(event: TEventType);
    private
      Spert: TSpert;
      Rig: TRig;
      SpertDatabase: TSpertDatabase;
      Configuration: TConfiguration;
      trxTunePower: Byte;
      trxPowerSavePoint: Byte;
  end;

implementation

constructor TEvent.Create(sp: TSpert; spdb: TSpertDatabase; r: TRig; c: TConfiguration);
begin
  Spert:=sp;
  SpertDatabase:=spdb;
  Rig:=r;
  Configuration:=c;

  trxTunePower:=Configuration.Settings.TrxTunePower;
  trxPowerSavePoint:=0;
end;

procedure TEvent.call(event: TEventType);
var
  enumVal: String;
begin
  WriteStr(enumVal, event);
  FormDebug.Log(Format('[Event] event %s - rig active %s', [enumVal, BoolToStr((Assigned(Rig) and Rig.isActive), True)]));


  case event of
    SpertEventBeforeTuneStart: begin
      { ATU tunning can set TRX power to 5W }
      if Rig.isActive and (trxTunePower > 0) and (trxPowerSavePoint = 0) and (not Rig.pttActive) then
      begin
        FormDebug.Log('[Event] setting trx power to: ' + IntToStr(trxTunePower));
        trxPowerSavePoint:=Rig.getPwr;
        Rig.SetPwr(trxTunePower);
      end;
    end;

    SpertEventAtferTuneStop: begin
      if Rig.isActive then SpertDatabase.insertAtuLC(Rig.getFrq, Spert.getAtu_L, Spert.getAtu_C, Spert.getAtu_K);
    end;

    SpertEventTxStop: begin
      if Rig.isActive and not Spert.isAtuActive then SpertDatabase.insertSwr(Rig.getFrq, Spert.getRpwrMaxPercent);
    end;

    RigEventTxStop: begin
      { restore trx power if atu stopped tuning - it's done on rig TX stop because we must be sure that no power greather than trxTunePower will be provided when SPert ATU is tuning }
      if (trxPowerSavePoint > 0) then
      begin
        FormDebug.Log('[Event] restoring trx power: ' + IntToStr(trxPowerSavePoint));
        Rig.SetPwr(trxPowerSavePoint);
      end;
      trxPowerSavePoint:=0;
    end;

    RigEventBandChange: begin
      if Spert.isAtuActive and Configuration.Settings.AtuOffOnBandChange
        then Spert.toggleAtuState;

      if Configuration.Settings.macroModeA and ContainsText(Configuration.Settings.macroModeAMod, Rig.getMode) and (Configuration.Settings.macroModeATrxPwr > 0)
        then Rig.SetPwr(Configuration.Settings.macroModeATrxPwr);

      if Configuration.Settings.macroModeB and ContainsText(Configuration.Settings.macroModeBMod, Rig.getMode) and (Configuration.Settings.macroModeBTrxPwr > 0)
        then Rig.SetPwr(Configuration.Settings.macroModeBTrxPwr);
    end;

    RigEventModeChange: begin
      if Spert.isAtuActive and Configuration.Settings.macroModeA and ContainsText(Configuration.Settings.macroModeAMod, Rig.getMode) and (Configuration.Settings.macroModeASpertPwr > 0)
        then Spert.setPower(Round(Configuration.Settings.macroModeASpertPwr / 10));

      if Spert.isAtuActive and Configuration.Settings.macroModeB and ContainsText(Configuration.Settings.macroModeBMod, Rig.getMode) and (Configuration.Settings.macroModeBSpertPwr > 0)
        then Spert.setPower(Round(Configuration.Settings.macroModeBSpertPwr / 10));
    end;
  end;
end;

end.

