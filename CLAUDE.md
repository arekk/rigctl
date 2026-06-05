# RadioShackCtl

Desktop control app for amateur-radio gear: Yaesu **FTdx10** / **FT-991A** transceivers and the **SPert 1000** HF power amplifier. Each device window works independently; with both connected, the app correlates radio frequency with stored SWR / ATU L-C memories.

Author: Arkadiusz Kuryłowicz (SO9W), Sugar Mice Software.

Public repository: https://github.com/arekk/rigctl

## Technology

- **Language / runtime:** Free Pascal (Object Pascal, `{$mode ObjFPC}{$H+}`) + Lazarus LCL.
- **Required packages** (see `rshackctr.lpi`): `SQLDBLaz`, `indylaz`, `TurboPowerIPro`, `LCL`.
- **Serial I/O:** patched `LazSynaSer` from https://github.com/JurassicPork/TLazSerial — vendored in repo (`lazsynaser.pas`, `lazsynautil.pas`, `lazsynafpc.pas`). Do not edit upstream copies casually.
- **Persistence:** SQLite (`SQLite3dyn` / `SQLite3Conn`) for the SPert SWR / ATU L-C memory database (`spert.db`); INI file (`radioshackctl.ini`) for settings.
- **HTTP / XML-RPC:** Indy (`TIdHTTPServer`) + `laz2_DOM` / `laz2_XMLRead` for the FLRIG-compatible XML-RPC server exposed to WSJT-X and logbooks.
- **Platform-specific:** macOS-only code paths use `{$IFDEF DARWIN}` and `{$modeswitch objectivec1}` + `CocoaAll` (e.g. `NSView(Handle).window.setFrameAutosaveName` for window-position persistence).

## Target platforms

- **Primary today:** macOS Apple Silicon / arm64 (migration target — was historically built for darwin x86_64).
- The `binaries/darwin_x86_64/` layout and `README.md` release recipe reflect the old Intel-build process; expect to update paths/recipe when ARM build is in place.
- `lpi` declares `Win32 GraphicApplication=True` but Windows/Linux are not actively built or tested.

## Project layout

Flat directory; one unit per file, no nested folders.

- `rshackctr.lpr` — program entry point; instantiates all forms in order: `FormInit`, `FormDebug`, `FormRig`, `FormSpert`, `FormSettings`.
- `unitforminit.pas` — bootstrap window (hidden 0×0). Owns `MainMenu`, wires the cross-device `TEvent` (IEvent implementation) between `FormSpert.Spert`, `FormRig.Rig`, `SpertDatabase` and `Configuration`. Shows/hides device windows based on settings.
- `unitformrig.pas` — radio window: VFO display, S-meter, band buttons, power slider, split / TXW / DNR / DNF / VOX toggles, keyboard shortcuts.
- `unitformspert.pas` — amplifier window: forward/reflected power, temperature, ATU on/off + tune, fan, TX lock, SWR/L-C history overlay (driven by `SpertDatabase`).
- `unitformsettings.pas` / `unitformdebug.pas` — settings dialog and debug log window (only visible when `Generic.Debug=true`).
- `unitrig.pas` — `TRig` interface (the contract all radios implement).
- `unitrigftdx10.pas` — FTdx10 implementation (`TFTdx10rig`); reference impl.
- `unitrigft991a.pas` — FT-991A (`TFT991Arig`); inherits from `TFTdx10rig`, overrides only what differs (`getSerialPortDiscoverParams`, disables split + drPortGain).
- `unitrigdummy.pas` — no-op `TRigDummy`, used when no radio is configured.
- `unitspert.pas` — `TSpert` (amplifier driver, single class — no interface, since only one amp is supported).
- `unitspertdatabase.pas` — `TSpertDatabase`: SQLite-backed memory of SWR samples and ATU L/C/K per (antenna, frequency-kHz).
- `unitflrigserver.pas` — `TFlrigServer`: Indy HTTP server speaking FLRIG XML-RPC on `127.0.0.1:12345` (default). Used by WSJT-X, logbooks. Includes a hamlib quirk fix in `HandleRequest`.
- `unitserialportautodiscover.pas` — `TSerialPortDiscover`: spawns one `TSerialPortDiscoverThread` per `/dev/tty.*`, sends an identifying probe, picks the one that answers, writes a lock-file alongside the INI so the other device's auto-discover skips it.
- `unitevents.pas` / `uniteventsinterface.pas` — `IEvent` + `TEvent`: cross-device reactions (e.g. on `RigEventBandChange`, turn off SPert ATU; on `SpertEventBeforeTuneStart`, drop TRX power to safe-tune level and restore on `RigEventTxStop`).
- `unitsettings.pas` — `TConfiguration` + `TSettings` record; INI-backed, mutex-guarded writes. Config dir: `~/.config/radioshackctl/` on macOS.

## Architecture conventions (must follow)

- **`{$mode ObjFPC}{$H+}` on every unit.** Forms that touch Cocoa add `{$modeswitch objectivec1}`.
- **Device state lives in a dedicated `TXxxState` class** with a `Reset` method (see `TSpertState`, `TFTdx10rigState`). The driver mutates the state; the rest of the app reads through driver getters — never reach into the state object from outside.
- **Driver → UI updates flow through `onState: TNotifyEvent`.** A periodic timer inside the driver polls the serial port and fires `onState(nil)`; the form's `UpdateUI` redraws the whole window each tick. Don't introduce per-property callbacks.
- **Cross-device reactions flow through `IEvent` (`UnitEvents`).** Drivers expose `onEvent(IEvent)`; `TEvent.call(TEventType)` is the single dispatcher that knows about both `Spert` and `Rig` plus `Configuration` and `SpertDatabase`. Add new event types to `TEventType` in `uniteventsinterface.pas` and handle them in `unitevents.pas`.
- **New radio models:** inherit from `TFTdx10rig` (CAT-protocol baseline) and override only what differs — `getSerialPortDiscoverParams` (to identify the rig on auto-discover) plus any unsupported feature (`SetSplit`, `SetdrPortGain`, etc., made into empty overrides). Pattern: see `TFT991Arig`.
- **Serial-port arbitration:** every long-lived connection uses `TBlockSerial` guarded by a `TCriticalSection` (`comportMutex`); the auto-discover writes per-port lock-files in the config directory so simultaneous SPert and Rig discoveries don't fight over the same `/dev/tty.*`.
- **`ignorePayload` (TXxxPayloadSuspend) is a deliberate pattern**, not a hack — it suppresses the echo of our own write until the radio confirms the new value, preventing UI flicker between optimistic state and stale-read.

## Language convention

- **All code, identifiers, comments and commit messages: English.**
- (Recent commits in Polish are legacy from before this rule was set; do not pattern-match on them.)

## Build & run

- **Day-to-day:** open `rshackctr.lpi` in Lazarus, F9 to build + run + debug.
- **CLI build:** `lazbuild rshackctr.lpi` (output: `binaries/$(TargetOS)_$(TargetCPU)/RadioShackCtl`).
- **macOS release packaging:** see the recipe block in `README.md` (`iconutil`, repacking `.app`, creating DMG via Disk Utility). The bundle path `binaries/darwin_x86_64/RadioShackCtl.app` is hard-coded in that recipe and will need updating for the arm64 target.
- **Runtime config:** `~/.config/radioshackctl/radioshackctl.ini` + `spert.db` + per-port `*.lockfile` artefacts.

## Roadmap / priorities

- **Migrate build to Apple Silicon / arm64** (was historically darwin x86_64).
- **Stability and bug-fixing**: general hardening of the existing feature set.
- **Add more radio models** beyond FTdx10 / FT-991A (use the inheritance pattern above).
- **Technical-debt cleanup** as it surfaces.
- **Maintenance** of existing features (FT-991A split is currently unsupported per README — "limited, w/o split" — open question whether this becomes a feature gap to close).

## External references

- SPert 1000 HF PA: https://rjk.com.pl/spert1000/
- Patched LazSynaSer upstream: https://github.com/JurassicPork/TLazSerial
- FLRIG XML-RPC protocol — the only documentation is the FLRIG source; this app implements a minimal subset sufficient for WSJT-X / hamlib (see method-name `case` in `unitflrigserver.pas`).
