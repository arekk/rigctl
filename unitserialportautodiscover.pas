unit UnitSerialPortAutoDiscover;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  LazSynaser,
  Cthreads,
  UnitFormDebug;

type
  TSerialPortDiscoverParams = class
    public
      constructor Create(handle: String; portRate: Integer; delimiter: String; send: String; expect: String);
    public
      fHandle: String;
      fPortRate: Integer;
      fDelimiter: String;
      fSend: String;
      fExpect: String;
  end;

  TSerialPortDiscoverResult = class
    Port: String;
    LockFile: String;
  end;

  TSerialPortDiscoverThread = class(TThread)
    private
      fPort: String;
      fParams: TSerialPortDiscoverParams;
      fLockFile: String;
      fResult: Integer;
    public
      constructor Create(port: String; params: TSerialPortDiscoverParams; lockFile: String);
      property Port: String read fPort;
      property LockFile: String read fLockFile;
      property Result: Integer read fResult;
    protected
      procedure Execute; override;
  end;

  TSerialPortDiscover = class
    private
      fLockFilesDirectory: String;
      fOnSuccess: TNotifyEvent;
      fOnFinish: TNotifyEvent;
      fAutoDiscoverThreads: Array of TSerialPortDiscoverThread;
    public
      constructor Create(LockFilesDirectory: String);
      procedure CleanLockFiles;
      function LockPort(port: String):String;
      function Discover(params: TSerialPortDiscoverParams): Integer;
      property OnSuccess: TNotifyEvent read fOnSuccess write fOnSuccess;
      property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    private
      procedure onAutoDiscoverThreadTerminate(Sender: TObject);
  end;

implementation

{ TSerialPortDiscoverParams }

constructor TSerialPortDiscoverParams.Create(handle: String; portRate: Integer; delimiter: String; send: String; expect: String);
begin
  fHandle:=handle;
  fPortRate:=portRate;
  fDelimiter:=delimiter;
  fSend:=send;
  fExpect:=expect;
end;

{ TSerialPortDiscover }

constructor TSerialPortDiscover.Create(LockFilesDirectory: String);
begin
  fLockFilesDirectory:=LockFilesDirectory;
end;

procedure TSerialPortDiscover.CleanLockFiles;
var
  searchResult: TSearchRec;
begin
  {$IFDEF DARWIN}
  if FindFirst(fLockFilesDirectory + PathDelim + '*.lockfile', faAnyFile, searchResult) = 0 then
    repeat DeleteFile(fLockFilesDirectory + PathDelim + searchResult.Name)
    until FindNext(searchResult) <> 0;
  {$ENDIF}
end;

function TSerialPortDiscover.LockPort(port: String):String;
var
  lockFile: String;
begin
  {$IFDEF DARWIN}
  lockFile:=fLockFilesDirectory + PathDelim + ExtractFileName(port) + '.lockfile';
  {$ELSE}
  lockFile:=fLockFilesDirectory + PathDelim + port + '.lockfile';
  {$ENDIF}
  if FileExists(lockFile) then DeleteFile(lockFile);
  FileCreate(lockFile);
  Result:=lockFile;
end;

function TSerialPortDiscover.Discover(params: TSerialPortDiscoverParams): Integer;
var
  searchResult: TSearchRec;
  i: Integer = 0;
begin
  {$IFDEF DARWIN}
  if FindFirst('/dev/tty.*', faAnyFile, searchResult) = 0 then begin repeat
    begin
      if (searchResult.Name <> 'tty.Bluetooth-Incoming-Port') and (searchResult.Name <> 'tty.wlan-debug') and (searchResult.Name <> 'tty.debug-console') then
      begin
        SetLength(fAutoDiscoverThreads, i + 1);
        fAutoDiscoverThreads[i]:=TSerialPortDiscoverThread.Create(('/dev/' + searchResult.Name), params, (fLockFilesDirectory + PathDelim + searchResult.Name + '.lockfile' ));
        fAutoDiscoverThreads[i].OnTerminate:=@onAutoDiscoverThreadTerminate;
        fAutoDiscoverThreads[i].Start;
        Inc(i);
        //Sleep(Random(500));
      end;
    end
      until FindNext(searchResult) <> 0;

    FindClose(searchResult);

    Result:=Length(fAutoDiscoverThreads);

    if Result > 0
      then FormDebug.Log('[PortDiscover][' + params.fHandle + '] waiting to ' + IntToStr(Result) + ' threads terminate')
      else FormDebug.Log('[PortDiscover][' + params.fHandle + '] no serial ports found');
  end;
  {$ENDIF}


end;

procedure TSerialPortDiscover.onAutoDiscoverThreadTerminate(Sender: TObject);
var
  thread: TSerialPortDiscoverThread;
  cnt: Integer = 0;
  result: TSerialPortDiscoverResult;
begin
  if (TSerialPortDiscoverThread(Sender).Result = 1) and Assigned(OnSuccess) then
  begin
    result:=TSerialPortDiscoverResult.Create;
    result.Port:=TSerialPortDiscoverThread(Sender).Port;
    result.LockFile:=TSerialPortDiscoverThread(Sender).LockFile;
    OnSuccess(result);
  end;

  for thread in fAutoDiscoverThreads do
    if thread.Result >= 0 then Inc(cnt);

  if Length(fAutoDiscoverThreads) = cnt then begin
    SetLength(fAutoDiscoverThreads, 0);
    if Assigned(OnFinish) then OnFinish(nil);
  end;
end;

{ TSerialPortDiscoverThread }

constructor TSerialPortDiscoverThread.Create(port: String; params: TSerialPortDiscoverParams; lockFile: String);
begin
  inherited Create(True);
  FreeOnTerminate:=False;

  fPort:=port;
  fParams:=params;
  fLockFile:=lockFile;

  fResult:=-1;
end;

procedure TSerialPortDiscoverThread.Execute;
var
  comport: TBlockSerial;
  payload: String;
begin
  if not FileExists(fLockFile) then
  begin
    FileCreate(fLockFile);

    comport:=TBlockSerial.Create;
    comport.LinuxLock:=False;
    comport.NonBlock:=True;
    comport.Connect(fPort);
    comport.Config(fParams.fPortRate, 8, 'N', SB1, False, True);
    FormDebug.Log('[PortDiscoverThread] execute -> ' + fParams.fHandle + ' -> ' + fPort +  ' -> ' + comport.LastErrorDesc + ' code: ' + Inttostr(comport.LastError));
    if comport.LastError = 0 then
    begin
      comport.SendString(fParams.fSend + fParams.fDelimiter);
      Sleep(300);
      payload:=Trim(AnsiToUtf8(comport.RecvTerminated(10, fParams.fDelimiter)));
      while payload <> '' do begin
        if ContainsText(payload, fParams.fExpect) then begin
          FormDebug.Log('[PortDiscoverThread] won ' + fParams.fHandle + ' -> ' + fPort);
          fResult:=1;
        end;
        payload:=Trim(AnsiToUtf8(comport.RecvTerminated(10, fParams.fDelimiter)));
      end;
    end;
    comport.CloseSocket;
    FreeAndNil(comport);

    if fResult = -1 then DeleteFile(fLockFile);
  end;

  if fResult = -1 then fResult:=0;

  FormDebug.Log('[PortDiscoverThread] terminate -> ' + fParams.fHandle + ' -> ' + fPort );
end;

end.

