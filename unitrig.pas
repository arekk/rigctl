unit UnitRig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UnitEventsInterface;

type
  TRigVFO = (RigVFO_A, RigVFO_B);

  TRigBand = (RigBand_160, RigBand_80, RigBand_60, RigBand_40, RigBand_30, RigBand_20, RigBand_17, RigBand_15, RigBand_12, RigBand_10, RigBand_6);

  TRigMode = (RigMode_USB, RigMode_LSB, RigMode_CW);

  TRig = interface
    procedure Start;
    procedure Stop;
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
    procedure playMessage(no: Byte);
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
    function getVfo:TRigVFO;
    function getMode:String;
    function getFrq:Longword;
    function getBand:Byte;
    function getdrPortGain: Byte;
    function getFPwrCur: Byte;
    function getFPwrMax: Byte;
    function getRPwrAvgPercent: Double;
    function getSMeter: String;
    function getVersion: String;
    procedure onEvent(event: IEvent);
    procedure onState(event: TNotifyEvent);
  end;

implementation

end.
