program RadioShackCtl;

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
  UnitFormInit,
  UnitFormDebug,
  UnitFormRig,
  UnitFormSpert,
  UnitFormSettings;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='RadioShackCtl';
  Application.Scaled:=True;
  Application.Initialize;

  Application.CreateForm(TFormInit, FormInit);
  Application.CreateForm(TFormDebug, FormDebug);
  Application.CreateForm(TFormRig, FormRig);
  Application.CreateForm(TFormSpert, FormSpert);
  Application.CreateForm(TFormSettings, FormSettings);

  Application.Run;
end.
