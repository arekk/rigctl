unit UnitRig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type
  TRigEventType = (RigEventBandChange, RigEventModeChange, RigEventTxStart, RigEventTxStop);

  TRigEvent = interface
    procedure call(event: TRigEventType);
  end;

  TRig = interface
    procedure setEventHandler(handler: TRigEvent);

    procedure Start;
    procedure Stop;
    procedure Send(payload: String);

    procedure SetVfoA_frq(frq: Longword);
    procedure SetVfoB_frq(frq: Longword);
    procedure SetVfoA_mode(mode: String);
    procedure SetVfoB_mode(mode: String);
    procedure SetSplit(active: Boolean);
    procedure SetPtt(active: Boolean);
    procedure SetCurrentVFOMode(mode: String);
    procedure SetPwr(pwr: Byte);
    procedure SetdrPortGain(gain: Byte);
    procedure toggleVox;
    procedure toggleDnr;
    procedure toggleDnf;
    procedure toggleTxw;
    procedure SetBand_160;
    procedure SetBand_80;
    procedure SetBand_60;
    procedure SetBand_40;
    procedure SetBand_30;
    procedure SetBand_20;
    procedure SetBand_17;
    procedure SetBand_15;
    procedure SetBand_12;
    procedure SetBand_10;
    procedure SetBand_6;

    function isActive:Boolean;
    function pttActive:Boolean;
    function splitActive:Boolean;
    function voxActive:Boolean;
    function dnrActive:Boolean;
    function dnfActive:Boolean;
    function txwActive:Boolean;
    function getPwr: Byte;
    function getVfoA_frq:Longword;
    function getVfoA_mode:String;
    function getVfoB_frq:Longword;
    function getVfoB_mode:String;
    function getVfo:Byte;
    function getMode:String;
    function getFrq:Longword;
    function getBand:Byte;
    function getdrPortGain: Byte;
    function getFPwrCur: Byte;
    function getFPwrMax: Byte;
    function getRPwrAvgPercent: Double;
    function getSMeter: String;
    function getVersion: String;
  end;

implementation

end.
