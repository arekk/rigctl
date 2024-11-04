program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  { you can add units after this }
  UnitSettings,

  UnitFormSpert,
  UnitFormRig,
  UnitFormDebug,
  UnitFormSettings,
  UnitFormInit;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='RigCtl';
  Application.Scaled:=True;
  Application.Initialize;

  { order is important }
  Application.CreateForm(TFormInit, FormInit);
  Application.CreateForm(TFormDebug, FormDebug);
  Application.CreateForm(TFormRig, FormRig);
  Application.CreateForm(TFormSpert, FormSpert);
  Application.CreateForm(TForm3, Form3);

  Application.Run;
end.

