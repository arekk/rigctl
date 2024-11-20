unit UnitEventsInterface;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type
  TEventType = (SpertEventBeforeTuneStart, SpertEventAtferTuneStop, SpertEventTxStart, SpertEventTxStop, RigEventBandChange, RigEventModeChange, RigEventTxStart, RigEventTxStop);

  IEvent = interface
    procedure call(event: TEventType);
  end;

implementation

end.

