unit UnitFlrigServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Forms,
  IdGlobal,
  IdSocketHandle,
  IdCustomHTTPServer,
  IdContext,
  IdHTTPServer,
  laz2_DOM,
  laz2_XMLRead,
  UnitFormDebug,
  UnitSettings,
  UnitRig;

type
  TFlrigServer = class
    public
      constructor Create(c: TConfiguration; r: TRig);
      destructor Destroy; override;
      procedure Start;
      procedure Stop;
    private
      procedure CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
      function HandleRequest(inputXML: String): String;
      { Deferred mutators — marshal Rig.SetXXX calls from the Indy worker thread
        to the main UI thread (where the rig's polling timer runs), so that all
        access to rigState/ignorePayload happens on a single thread. }
      procedure deferredSetVfoA(data: PtrInt);
      procedure deferredSetVfoB(data: PtrInt);
      procedure deferredSetSplit(data: PtrInt);
      procedure deferredSetMode(data: PtrInt);
      procedure deferredSetPtt(data: PtrInt);
      procedure deferredPlayMessage(data: PtrInt);
    private
      Rig: TRig;
      Server: TIdHTTPServer;
      Configuration: TConfiguration;
    private
      const XML_CMD_REPLY   ='<?xml version="1.0"?><methodResponse><params><param><value></value></param></params></methodResponse>';
      const XML_STR_REPLY   ='<?xml version="1.0"?><methodResponse><params><param><value>%s</value></param></params></methodResponse>';
      const XML_INT_REPLY   ='<?xml version="1.0"?><methodResponse><params><param><value><i4>%d</i4></value></param></params></methodResponse>';
      const XML_MODES_REPLY ='<?xml version="1.0"?><methodResponse><params><param><value><array><data><value>LSB</value><value>USB</value><value>CW-U</value><value>FM</value><value>AM</value><value>RTTY-L</value><value>CW-L</value><value>DATA-L</value><value>RTTY-U</value><value>DATA-FM</value><value>FM-N</value><value>DATA-U</value><value>AM-N</value><value>PSK</value><value>DATA-FMN</value></data></array></value></param></params></methodResponse>';
      const XML_BW_REPLY    ='<?xml version="1.0"?><methodResponse><params><param><value><array><data><value>3000</value><value></value></data></array></value></param></params></methodResponse>';
      const XML_FAULT_REPLY ='<?xml version="1.0"?><methodResponse><fault><value><struct><member><name>faultCode</name><value><i4>-1</i4></value></member><member><name>faultString</name><value>%s</value></member></struct></value></fault></methodResponse>';
  end;

implementation

{ THTTPServerThread }

constructor TFlrigServer.Create(c: TConfiguration; r: TRig);
begin
  Configuration:=c;

  Rig:=r;

  Server:=TIdHTTPServer.Create(nil);
  Server.KeepAlive:=True;
  Server.ListenQueue:=50;
  Server.MaxConnections:=5;
end;

procedure TFlrigServer.Start;
var
  SHandle: TIdSocketHandle;
begin
  Configuration.Load;

  if Configuration.Settings.flrigServerEnabled then
  begin
    FormDebug.Log('[FlrigServer] starting on port ' + IntToStr(Configuration.Settings.flrigServerPort));

    Server.OnCommandGet:=@CommandGet;

    try
       Server.Bindings.Clear;
       SHandle := Server.Bindings.Add;
       SHandle.IP := '127.0.0.1';
       SHandle.Port := Configuration.Settings.flrigServerPort;
       Server.Active := True;
     except
       on E: Exception do FormDebug.Log('[FlrigServer] start exception - ' + E.Message);
     end;
  end
  else
    FormDebug.Log('[FlrigServer] server disabled, not starting');
end;

procedure TFlrigServer.CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Stream: TStream;
  Output: String;
begin
  AResponseInfo.CharSet:='UTF-8';

  if (ARequestInfo.Command = 'POST') then
  begin
    Output:='<?xml version="1.0"?><methodResponse><fault><value><struct><member><name>faultCode</name><value><i4>-1</i4></value></member><member><name>faultString</name><value>Malformed request - missing POST payload or Content-Type header</value></member></struct></value></fault></methodResponse>';
    Stream := ARequestInfo.PostStream;
    if assigned(Stream) then
    begin
      Stream.Position := 0;
      Output:=HandleRequest(ReadStringFromStream(Stream));
    end;

    AResponseInfo.ContentText := Output + #10;
    AResponseInfo.ContentType:='text/xml';
  end
  else
  begin
    FormDebug.Log('[FlrigServer] 404 on ' + ARequestInfo.Document + ' method ' + ARequestInfo.Command);
    AResponseInfo.ContentText := '404' + #10;
    AResponseInfo.ResponseNo:=404;
    AResponseInfo.ContentType:='text/plain';
  end;
end;

function TFlrigServer.HandleRequest(inputXML: String): String;
var
  xmlStr: String;
  xmlDoc: TXMLDocument;
  xmlNode: TDOMNode;
  xmlStream: TStringStream;
  command: String;
  CurrentOutput: String;
  modeStr: PString;
begin
  xmlDoc:=nil;
  xmlNode:=nil;
  xmlStream:=nil;
  try
    try
      xmlStr:=ReplaceText(inputXML, '<?clientid="hamlib', '<?client id="hamlib'); // fix for malformed XML from WSJT-X (via hamlib)
      xmlStream:=TStringStream.Create(xmlStr);
      ReadXMLFile(xmlDoc, xmlStream);
      xmlNode:=xmlDoc.DocumentElement.FirstChild;
    except
       on E: Exception do FormDebug.Log('[FlrigServer] xml exception - ' + E.Message + ' - XML was: ' + xmlStr);
    end;

  if Assigned(xmlNode) then
  begin
    command:=xmlNode.TextContent;
    begin
      case command of
        'main.get_version': CurrentOutput:=Format(XML_STR_REPLY, ['2.0.05']);

        'rig.get_xcvr': CurrentOutput:=Format(XML_STR_REPLY, ['FTDX10']);

        'rig.get_pwrmeter_scale': CurrentOutput:=Format(XML_INT_REPLY, [1]);

        'rig.get_modeA': CurrentOutput:=Format(XML_STR_REPLY, [Rig.getVfoA_mode]);

        'rig.get_modeB': CurrentOutput:=Format(XML_STR_REPLY, [Rig.getVfoB_mode]);

        'rig.get_bwA': CurrentOutput:=XML_BW_REPLY;

        'rig.get_bwB': CurrentOutput:=XML_BW_REPLY;

        'rig.get_AB': begin
          case Rig.getVfo of
            RigVFO_A: CurrentOutput:=Format(XML_STR_REPLY, ['A']);
            RigVFO_B: CurrentOutput:=Format(XML_STR_REPLY, ['B']);
          end;
        end;

        'rig.get_ptt': begin
          if Rig.pttActive
            then CurrentOutput:=Format(XML_INT_REPLY, [1])
            else CurrentOutput:=Format(XML_INT_REPLY, [0])
        end;

        'rig.get_mode': CurrentOutput:=Format(XML_STR_REPLY, [Rig.getMode]);

        'rig.get_split': begin
          if Rig.splitActive
            then CurrentOutput:=Format(XML_INT_REPLY, [1])
            else CurrentOutput:=Format(XML_INT_REPLY, [0])
        end;

        'rig.get_modes':CurrentOutput:=XML_MODES_REPLY;

        'rig.get_vfoA': CurrentOutput:=Format(XML_STR_REPLY, [IntToStr(Rig.getVfoA_frq)]);

        'rig.get_vfoB': CurrentOutput:=Format(XML_STR_REPLY, [IntToStr(Rig.getVfoB_frq)]);

        'rig.set_vfoA': begin
          xmlNode := xmlNode.NextSibling;
          if Assigned(xmlNode) then
            // 28_616_781.250000 -> 28_616_781 (part after period is not important)
            Application.QueueAsyncCall(@deferredSetVfoA, PtrInt(StrToInt(Copy2Symb(xmlNode.TextContent, '.'))));
          CurrentOutput:=XML_CMD_REPLY;
        end;

        'rig.set_vfoB': begin
          xmlNode := xmlNode.NextSibling;
          if Assigned(xmlNode) then
            Application.QueueAsyncCall(@deferredSetVfoB, PtrInt(StrToInt(Copy2Symb(xmlNode.TextContent, '.'))));
          CurrentOutput:=XML_CMD_REPLY;
        end;

        'rig.set_split': begin
           xmlNode := xmlNode.NextSibling;
           if Assigned(xmlNode) then
             Application.QueueAsyncCall(@deferredSetSplit, PtrInt(Ord(xmlNode.TextContent = '1')));
           CurrentOutput:=XML_CMD_REPLY
        end;

        'rig.set_mode': begin
           xmlNode := xmlNode.NextSibling;
           if Assigned(xmlNode) then
           begin
             { String passed via heap-allocated PString; deferredSetMode disposes it. }
             New(modeStr);
             modeStr^ := xmlNode.TextContent;
             Application.QueueAsyncCall(@deferredSetMode, PtrInt(modeStr));
           end;
           CurrentOutput:=XML_CMD_REPLY
        end;

        'rig.set_ptt': begin
           xmlNode := xmlNode.NextSibling;
           if Assigned(xmlNode) then
             Application.QueueAsyncCall(@deferredSetPtt, PtrInt(Ord(xmlNode.TextContent = '1')));
           CurrentOutput:=XML_CMD_REPLY
        end;

        // non-standard method
        'rig.play_message': begin
           xmlNode := xmlNode.NextSibling;
           if Assigned(xmlNode) then
             Application.QueueAsyncCall(@deferredPlayMessage, PtrInt(StrToInt(xmlNode.TextContent)));
           CurrentOutput:=XML_CMD_REPLY
        end;
      end;

      if CurrentOutput='' then begin
        FormDebug.Log('[FlrigServer] command not handled: ' + command);
        CurrentOutput:=Format(XML_FAULT_REPLY, [command + ': unknown method name']);
      end;
    end;
  end;

    Result:=CurrentOutput;
  finally
    xmlDoc.Free;
    xmlStream.Free;
  end;
end;

procedure TFlrigServer.Stop;
begin
  Server.Active:=False;
  FormDebug.Log('[FlrigServer] server stopped');
end;

destructor TFlrigServer.Destroy;
begin
  // Drop any pending deferred mutations queued from Indy worker threads — they
  // would otherwise fire after this instance is gone.
  Application.RemoveAsyncCalls(Self);
  FreeAndNil(Server);
  // Configuration and Rig are borrowed references owned by TFormRig — do not free here.
  inherited;
end;

procedure TFlrigServer.deferredSetVfoA(data: PtrInt);
begin
  Rig.SetVfoA_frq(Longword(data));
end;

procedure TFlrigServer.deferredSetVfoB(data: PtrInt);
begin
  Rig.SetVfoB_frq(Longword(data));
end;

procedure TFlrigServer.deferredSetSplit(data: PtrInt);
begin
  Rig.SetSplit(data <> 0);
end;

procedure TFlrigServer.deferredSetMode(data: PtrInt);
var
  s: PString;
begin
  s := PString(data);
  try
    Rig.SetCurrentVFOMode(s^);
  finally
    Dispose(s);
  end;
end;

procedure TFlrigServer.deferredSetPtt(data: PtrInt);
begin
  Rig.SetPtt(data <> 0);
end;

procedure TFlrigServer.deferredPlayMessage(data: PtrInt);
begin
  Rig.playMessage(Byte(data));
end;

end.
