unit UnitFlrigServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
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
    private
      Rig: TRig;
      Server: TIdHTTPServer;
      Configuration: TConfiguration;
    private
      const XML_CMD_REPLY   ='<?xml version="1.0"?><methodResponse><params><param><value></value></param></params></methodResponse>';
      const XML_STR_REPLY   ='<?xml version="1.0"?><methodResponse><params><param><value>%s</value></param></params></methodResponse>';
      const XML_INT_REPLY   ='<?xml version="1.0"?><methodResponse><params><param><value><i4>%d</i4></value></param></params></methodResponse>';
      const XML_MODES_REPLY ='<?xml version="1.0"?><methodResponse><params><param><value><array><data><value>LSB</value><value>USB</value><value>CW-U</value><value>FM</value><value>AM</value><value>RTTY-L</value><value>CW-L</value><value>DATA-L</value><value>RTTY-U</value><value>DATA-FM</value><value>FM-N</value><value>DATA-U</value><value>AM-N</value><value>PSK</value><value>DATA-FMN</value></data></array></value></param></params></methodResponse>';
      const XML_BW_REPLY    ='<?xml version="1.0"?><methodResponse><params><param><value><array><data><value>3000</value><value></value></data></array></value></param></params></methodResponse></methodResponse>';
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
  AResponseInfo.ContentEncoding:='UTF-8';
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
  command: String;
  CurrentOutput: String;
begin
  try
    xmlStr:=ReplaceText(inputXML, '<?clientid="hamlib', '<?client id="hamlib'); // fix for malformed XML from WSJT-X (via hamlib)
    ReadXMLFile(xmlDoc, TStringStream.Create(xmlStr));
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
          if Rig.getVfo = 0
            then CurrentOutput:=Format(XML_STR_REPLY, ['A'])
            else CurrentOutput:=Format(XML_STR_REPLY, ['B'])
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
          if Assigned(xmlNode) then Rig.SetVfoA_frq(StrToInt(Copy2Symb(xmlNode.TextContent, '.'))); // 28_616_781.250000 -> 28_616_781 (part after period is not important)
          CurrentOutput:=XML_CMD_REPLY;
        end;

        'rig.set_vfoB': begin
          xmlNode := xmlNode.NextSibling;
          if Assigned(xmlNode) then Rig.SetVfoB_frq(StrToInt(Copy2Symb(xmlNode.TextContent, '.')));
          CurrentOutput:=XML_CMD_REPLY;
        end;

        'rig.set_split': begin
           xmlNode := xmlNode.NextSibling;
           if Assigned(xmlNode) then Rig.SetSplit((xmlNode.TextContent = '1'));
           CurrentOutput:=XML_CMD_REPLY
        end;

        'rig.set_mode': begin
           xmlNode := xmlNode.NextSibling;
           if Assigned(xmlNode) then Rig.SetCurrentVFOMode(xmlNode.TextContent);
           CurrentOutput:=XML_CMD_REPLY
        end;

        'rig.set_ptt': begin
           xmlNode := xmlNode.NextSibling;
           if Assigned(xmlNode) then Rig.SetPtt((xmlNode.TextContent = '1'));
           CurrentOutput:=XML_CMD_REPLY
        end;

        // non-standard method
        'rig.play_message': begin
           xmlNode := xmlNode.NextSibling;
           if Assigned(xmlNode) then Rig.playMessage(StrToInt(xmlNode.TextContent));
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
end;

procedure TFlrigServer.Stop;
begin
  Server.Active:=False;
  FormDebug.Log('[FlrigServer] server stopped');
end;

destructor TFlrigServer.Destroy;
begin
  FreeAndNil(Server);
  FreeAndNil(Configuration);
  inherited;
end;

end.
