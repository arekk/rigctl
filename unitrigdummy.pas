unit UnitRigDummy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  UnitRig,
  UnitEventsInterface;

type
  TRigDummy = class(TInterfacedObject, TRig)
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

procedure TRigDummy.Start; begin end;

procedure TRigDummy.Stop; begin end;

procedure TRigDummy.SetVfoA_frq(frq: Longword); begin end;

procedure TRigDummy.SetVfoB_frq(frq: Longword); begin end;

procedure TRigDummy.SetVfoA_mode(mode: String); begin end;

procedure TRigDummy.SetVfoB_mode(mode: String); begin end;

procedure TRigDummy.SetSplit(active: Boolean); begin end;

procedure TRigDummy.SetPtt(active: Boolean); begin end;

procedure TRigDummy.SetCurrentVFOMode(mode: String); begin end;

procedure TRigDummy.SetPwr(pwr: Byte); begin end;

procedure TRigDummy.SetdrPortGain(gain: Byte); begin end;

procedure TRigDummy.toggleVox; begin end;

procedure TRigDummy.toggleDnr; begin end;

procedure TRigDummy.toggleDnf; begin end;

procedure TRigDummy.toggleTxw; begin end;

procedure TRigDummy.SetBand_160; begin end;

procedure TRigDummy.SetBand_80; begin end;

procedure TRigDummy.SetBand_60; begin end;

procedure TRigDummy.SetBand_40; begin end;

procedure TRigDummy.SetBand_30; begin end;

procedure TRigDummy.SetBand_20; begin end;

procedure TRigDummy.SetBand_17; begin end;

procedure TRigDummy.SetBand_15; begin end;

procedure TRigDummy.SetBand_12; begin end;

procedure TRigDummy.SetBand_10; begin end;

procedure TRigDummy.SetBand_6; begin end;

procedure TRigDummy.playMessage(no: Byte); begin end;

function TRigDummy.isActive:Boolean;
begin
  Result:=False;
end;

function TRigDummy.pttActive:Boolean;
begin
  Result:=False;
end;

function TRigDummy.splitActive:Boolean;
begin
  Result:=False;
end;

function TRigDummy.voxActive:Boolean;
begin
  Result:=False;
end;

function TRigDummy.dnrActive:Boolean;
begin
  Result:=False;
end;

function TRigDummy.dnfActive:Boolean;
begin
  Result:=False;
end;

function TRigDummy.txwActive:Boolean;
begin
  Result:=False;
end;

function TRigDummy.getPwr: Byte;
begin
  Result:=0;
end;

function TRigDummy.getVfoA_frq:Longword;
begin
  Result:=0;
end;

function TRigDummy.getVfoA_mode:String;
begin
  Result:='';
end;

function TRigDummy.getVfoB_frq:Longword;
begin
  Result:=0;
end;

function TRigDummy.getVfoB_mode:String;
begin
  Result:='';
end;

function TRigDummy.getVfo:TRigVFO;
begin
  Result:=RigVFO_A;
end;

function TRigDummy.getMode:String;
begin
  Result:='';
end;

function TRigDummy.getFrq:Longword;
begin
  Result:=0;
end;

function TRigDummy.getBand:Byte;
begin
  Result:=160;
end;

function TRigDummy.getdrPortGain: Byte;
begin
  Result:=0;
end;

function TRigDummy.getFPwrCur: Byte;
begin
  Result:=0;
end;

function TRigDummy.getFPwrMax: Byte;
begin
  Result:=0;
end;

function TRigDummy.getRPwrAvgPercent: Double;
begin
  Result:=0;
end;

function TRigDummy.getSMeter: String;
begin
  Result:='0';
end;

function TRigDummy.getVersion: String;
begin
  Result:='dummy';
end;

procedure TRigDummy.onEvent(event: IEvent); begin end;

procedure TRigDummy.onState(event: TNotifyEvent); begin end;

end.

