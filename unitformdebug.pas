unit UnitFormDebug;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes,
  SysUtils,
  Forms,
  CocoaAll,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  UnitSettings;

type

  { TFormDebug }

  TFormDebug = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Log(msg: String);
  private
    LastLogMessage: String;
    Configuration: TConfiguration;
  private
    procedure LogProc;
  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.lfm}

{ TFormDebug }

procedure TFormDebug.Log(msg: String);
begin
  LastLogMessage:=msg;
  TThread.Synchronize(nil, @LogProc);
end;

procedure TFormDebug.LogProc;
begin
  if (LastLogMessage <> '') and CheckBox1.Checked then
  begin
    Memo1.Lines.BeginUpdate;
    Memo1.Lines.Add(LastLogMessage);
    Memo1.Lines.EndUpdate;
  end;
  LastLogMessage:=''
end;

procedure TFormDebug.FormCreate(Sender: TObject);
begin
  Configuration:=TConfiguration.Create(Application.Location);
  Configuration.Load;

  Visible:=Configuration.Settings.Debug;
end;

procedure TFormDebug.FormShow(Sender: TObject);
begin
  NSView(Handle).window.setFrameAutosaveName(NSSTR(ClassName));
  CheckBox1.Checked:=True;
  Log('Current Directory is : ' + ExtractFilePath(Application.Location));
end;

procedure TFormDebug.Button1Click(Sender: TObject);
begin
  FormDebug.Memo1.Clear;
end;

procedure TFormDebug.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CheckBox1.Checked:=False;
  FormDebug.Memo1.Clear;
  CloseAction:=caHide;
end;

procedure TFormDebug.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Configuration);
end;

end.

