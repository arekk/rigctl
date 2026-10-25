unit UnitEvents;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  ExtCtrls,
  UnitEventsInterface,
  UnitSettings,
  UnitSpertDatabase,
  UnitSpert,
  UnitRig,
  UnitFormDebug;

const
  { Gain-control parameters. After TxStart in the right conditions, ramp PortGain
    one step per tick until Spert.getFpwr lands in the ±10% deadband around target
    — then freeze for the rest of this TX session. A single drPortGain step on the
    Yaesu moves output power ~20-25%, so chasing fluctuations after target would
    just oscillate. The next TxStart re-evaluates from the converged gain. Band
    change resets gain to GAIN_RAMP_START. }
  GAIN_RAMP_START   = 10;
  GAIN_RAMP_MAX     = 100;
  GAIN_RAMP_TICK_MS = 200;     { matches default Spert poll cadence }
  GAIN_DEADBAND_PCT = 0.10;    { ±10% — once fpwr lands in this band, regulation stops for this TX }

type
  TEvent = class(TInterfacedObject, IEvent)
    public
      constructor Create(sp: TSpert; spdb: TSpertDatabase; r: TRig; c: TConfiguration);
      destructor Destroy; override;
      procedure call(event: TEventType);
    private
      Spert: TSpert;
      Rig: TRig;
      SpertDatabase: TSpertDatabase;
      Configuration: TConfiguration;
      trxTunePower: Byte;
      trxPowerSavePoint: Byte;
      { Gain-control state. The timer runs from TxStart until fpwr converges into
        the deadband (or hits the gain ceiling/floor), then stops itself for the
        rest of the TX. Next TxStart restarts it. }
      gainRampTarget: Integer;
      gainRampCurrent: Integer;
      gainRampActive: Boolean;
      gainRampTimer: TTimer;
    private
      procedure gainRampTryStart;
      procedure gainRampStop(reason: String);
      procedure onGainRampTick(Sender: TObject);
  end;

implementation

{ Implementation-section uses — UnitFormRig.interface does not depend on
  UnitEvents, so this does not create a circular dependency. We need FormRig
  here purely to read BitBtnPortGainAuto.Tag as the mechanism's gate. }
uses
  UnitFormRig;

{ Macro mode matching against canonical rig categories (PHONO / DATA / CW).
  The rig driver (TFTdx10rig.getModeCategory) maps its native mode strings
  to one of these; the macro config stores one of these. Match = both
  non-empty and equal. Empty macroCategory or unmapped rig mode → no match. }
function modeMatches(macroCategory: String; rigCategory: String): Boolean;
begin
  Result := (macroCategory <> '') and (macroCategory = rigCategory);
end;

{ Returns the gain-control target if the auto mechanism is engaged for the
  current state, else 0.

  Activation requirements (all must hold):
    - BitBtnPortGainAuto on TFormRig is toggled ON (Tag = 1)
    - Rig is in DATA mode (getModeCategory = DATA)
    - SPert has a non-zero ALC power (getPwr > 0) — without it the target would be 0

  Target value is Spert.getPwr * 10, i.e. the current SPert set-power expressed
  in watts (since getPwr is the ALC_PWM scale 0..100). }
function autoPortGainTarget(rig: TRig; spert: TSpert): Integer;
begin
  Result := 0;
  if (rig = nil) or (spert = nil) then Exit;
  if not Assigned(FormRig) or not Assigned(FormRig.BitBtnPortGainAuto) then Exit;
  if FormRig.BitBtnPortGainAuto.Tag <> 1 then Exit;
  if rig.getModeCategory <> RigModeCategory_DATA then Exit;
  if spert.getPwr <= 0 then Exit;
  Result := spert.getPwr * 10;
end;

constructor TEvent.Create(sp: TSpert; spdb: TSpertDatabase; r: TRig; c: TConfiguration);
begin
  Spert:=sp;
  SpertDatabase:=spdb;
  Rig:=r;
  Configuration:=c;

  trxTunePower:=Configuration.Settings.TrxTunePower;
  trxPowerSavePoint:=0;

  gainRampTimer := TTimer.Create(nil);
  gainRampTimer.Enabled := False;
  gainRampTimer.Interval := GAIN_RAMP_TICK_MS;
  gainRampTimer.OnTimer := @onGainRampTick;
  gainRampActive := False;
  gainRampTarget := 0;
  gainRampCurrent := 0;
end;

destructor TEvent.Destroy;
begin
  FreeAndNil(gainRampTimer);
  inherited;
end;

procedure TEvent.gainRampTryStart;
var
  target: Integer;
begin
  if gainRampActive then Exit;

  target := autoPortGainTarget(Rig, Spert);
  if target <= 0 then Exit;

  { Pick up current radio gain — band-change reset (see RigEventBandChange) sets
    it to GAIN_RAMP_START; otherwise we continue from wherever the last TX cycle
    converged, which is usually already inside the deadband. }
  gainRampTarget := target;
  gainRampCurrent := Rig.getDrPortGain;
  gainRampActive := True;
  gainRampTimer.Enabled := True;
  FormDebug.Log(Format('[Event][AutoGain] start, target=%d, gain=%d', [gainRampTarget, gainRampCurrent]));
end;

procedure TEvent.gainRampStop(reason: String);
begin
  if not gainRampActive then Exit;
  gainRampTimer.Enabled := False;
  gainRampActive := False;
  FormDebug.Log(Format('[Event][AutoGain] stop (%s) gain=%d fpwr=%d', [reason, gainRampCurrent, Spert.getFpwr]));
end;

procedure TEvent.onGainRampTick(Sender: TObject);
var
  fpwr: Integer;
  deadbandLower, deadbandUpper: Integer;
begin
  if not gainRampActive then Exit;

  fpwr := Spert.getFpwr;
  deadbandLower := Round(gainRampTarget * (1 - GAIN_DEADBAND_PCT));
  deadbandUpper := Round(gainRampTarget * (1 + GAIN_DEADBAND_PCT));

  { Once fpwr lands in the ±10% target deadband, stop adjusting for this TX
    session — PortGain doesn't get re-evaluated until TxStop → next TxStart.
    A single Yaesu drPortGain step is ~20-25% on output, so re-acting on every
    fluctuation would oscillate. Per-session "set and hold" is the right model. }
  if (fpwr >= deadbandLower) and (fpwr <= deadbandUpper) then
  begin
    gainRampTimer.Enabled := False;
    gainRampActive := False;
    FormDebug.Log(Format('[Event][AutoGain] converged, gain=%d fpwr=%d target=%d', [gainRampCurrent, fpwr, gainRampTarget]));
    Exit;
  end;

  if (fpwr < deadbandLower) and (gainRampCurrent < GAIN_RAMP_MAX) then
  begin
    Inc(gainRampCurrent);
    Rig.SetdrPortGain(gainRampCurrent);
    FormDebug.Log(Format('[Event][AutoGain] +gain=%d fpwr=%d target=%d', [gainRampCurrent, fpwr, gainRampTarget]));
  end
  else if (fpwr > deadbandUpper) and (gainRampCurrent > 0) then
  begin
    Dec(gainRampCurrent);
    Rig.SetdrPortGain(gainRampCurrent);
    FormDebug.Log(Format('[Event][AutoGain] -gain=%d fpwr=%d target=%d', [gainRampCurrent, fpwr, gainRampTarget]));
  end
  else
  begin
    { Hit the ceiling (gain=100, still too low) or floor (gain=0, still too high)
      — antenna mismatch is unrecoverable. Give up for this TX. }
    gainRampTimer.Enabled := False;
    gainRampActive := False;
    FormDebug.Log(Format('[Event][AutoGain] gave up at gain=%d fpwr=%d target=%d', [gainRampCurrent, fpwr, gainRampTarget]));
  end;
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

    SpertEventTxStart: begin
      gainRampTryStart;
    end;

    SpertEventTxStop: begin
      gainRampStop('tx stop');

      { Threshold on getFpwrMax filters out brief PTT taps / key clicks where no real
        power was measured. Without this, getRpwrMaxPercent returns 0 (fpwrMax=0 case)
        and a synthetic "0% SWR" sample gets stored, polluting the averaged history
        and weakening the lock-on-SWR safety check.
        10 is empirical — well above noise floor, well below any real TX. }
      if Rig.isActive and not Spert.isAtuActive and (Spert.getFpwrMax >= 10) then
        SpertDatabase.insertSwr(Rig.getFrq, Spert.getRpwrMaxPercent);
    end;

    RigEventTxStop: begin
      { restore trx power if atu stopped tuning - it's done on rig TX stop because we
        must be sure that no power greather than trxTunePower will be provided when SPert
        ATU is tuning }
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

      { Per the spec — antenna behavior changes per band, so the previously-converged
        gain is no longer valid. Stop any active ramp and immediately set PortGain
        back to GAIN_RAMP_START. Only do this when the auto mechanism is currently
        engaged (button toggled + DATA + SPert set-power > 0), otherwise we'd
        clobber the user's manual gain choice in PHONO / CW modes. }

      if gainRampActive then gainRampStop('band change');
      if autoPortGainTarget(Rig, Spert) > 0 then
      begin
        gainRampCurrent := GAIN_RAMP_START;
        Rig.SetdrPortGain(GAIN_RAMP_START);
        FormDebug.Log('[Event][AutoGain] band change, gain reset to ' + IntToStr(GAIN_RAMP_START));
      end;
    end;

    RigEventModeChange: begin
      FormDebug.Log(Format('[Event] new rig mode %s (%s)', [Rig.getMode, Rig.getModeCategory]));

      if Configuration.Settings.macroModeA and
         modeMatches(Configuration.Settings.macroModeAMod, Rig.getModeCategory) and
         (Configuration.Settings.macroModeATrxPwr > 0) then
      begin
        FormDebug.Log(Format('[Event][Macro A] Rig.SetPwr(%d)', [Configuration.Settings.macroModeATrxPwr]));
        Rig.SetPwr(Configuration.Settings.macroModeATrxPwr);
      end;

      if Configuration.Settings.macroModeB and
         modeMatches(Configuration.Settings.macroModeBMod, Rig.getModeCategory) and
         (Configuration.Settings.macroModeBTrxPwr > 0) then
      begin
        FormDebug.Log(Format('[Event][Macro B] Rig.SetPwr(%d)', [Configuration.Settings.macroModeBTrxPwr]));
        Rig.SetPwr(Configuration.Settings.macroModeBTrxPwr);
      end;

      if Spert.isActive and
         Configuration.Settings.macroModeA and
         modeMatches(Configuration.Settings.macroModeAMod, Rig.getModeCategory) and
         (Configuration.Settings.macroModeASpertPwr > 0) then
      begin
        FormDebug.Log(Format('[Event][Macro A] Spert.setPower(%d)', [Round(Configuration.Settings.macroModeASpertPwr / 10)]));
        Spert.setPower(Round(Configuration.Settings.macroModeASpertPwr / 10));
      end;

      if Spert.isActive and
         Configuration.Settings.macroModeB and
         modeMatches(Configuration.Settings.macroModeBMod, Rig.getModeCategory) and
         (Configuration.Settings.macroModeBSpertPwr > 0) then
      begin
        FormDebug.Log(Format('[Event][Macro B] Spert.setPower(%d)', [Round(Configuration.Settings.macroModeBSpertPwr / 10)]));
        Spert.setPower(Round(Configuration.Settings.macroModeBSpertPwr / 10));
      end;
    end;
  end;
end;

end.

