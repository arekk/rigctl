unit UnitSpertDatabase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Dateutils,
  SQLdb,
  SQLite3dyn,
  SQLite3Conn,
  UnitSettings,
  UnitFormDebug;

type
  TSpertDatabaseLCmatch = (SpertDatabaseLC_Unknown, SpertDatabaseLC_Match, SpertDatabaseLC_NotMatch);

  TSpertDatabase = class
    public
      constructor Create(Config: TConfiguration);
      destructor Destroy; override;
      procedure Open;
      procedure Close;
      procedure Clear;
      procedure insertSwr(frequency: Longword; swr: Double);
      procedure insertAtuLC(frequency: Longword; L, C: Double; K: Byte);
      function estimateSwr(frequency: Longword; span: Integer): Double;
      function matchLC(frequency: Longword; span: Integer; L, C: Double): TSpertDatabaseLCMatch;
    private
      Configuration: TConfiguration;
      databaseConnection: TSQLite3Connection;
      databaseTransaction: TSQLTransaction;
      databaseQuery: TSQLQuery;
      active: Boolean;
    private
      procedure setUpDatabase;
  end;

implementation

constructor TSpertDatabase.Create(Config: TConfiguration);
begin
  Configuration:=Config;

  databaseTransaction:=TSQLTransaction.Create(nil);

  databaseConnection:=TSQLite3Connection.Create(nil);
  databaseConnection.Transaction:=databaseTransaction;
  databaseConnection.DatabaseName:=Configuration.getConfigDirectory + PathDelim + 'spert.db';

  databaseQuery:=TSQLQuery.Create(nil);
  databaseQuery.Transaction:=databaseTransaction;
  databaseQuery.Database := databaseConnection;

  active:=False;
end;

procedure TSpertDatabase.Open;
begin
  databaseConnection.Open;
  active:=True;
  setUpDatabase;
end;

procedure TSpertDatabase.Close;
begin
  active:=False;
  databaseConnection.Close(True);
end;

destructor TSpertDatabase.Destroy;
begin
  FreeAndNil(databaseQuery);
  FreeAndNil(databaseConnection);
  FreeAndNil(databaseTransaction);
end;

procedure TSpertDatabase.setUpDatabase;
begin
  if active then
  begin
    databaseTransaction.StartTransaction;

    databaseConnection.ExecuteDirect('CREATE TABLE IF NOT EXISTS "swr_samples" (antenna TEXT NOT NULL, frequency INTEGER NOT NULL, swr REAL, ts INTEGER)');
    databaseConnection.ExecuteDirect('CREATE INDEX IF NOT EXISTS "swr_samples_antenna" ON  "swr_samples" (antenna, frequency)');

    databaseConnection.ExecuteDirect('CREATE TABLE IF NOT EXISTS "atu_LC_samples" (antenna TEXT NOT NULL, frequency INTEGER NOT NULL, L REAL, C REAL, K INTEGER, ts INTEGER)');
    databaseConnection.ExecuteDirect('CREATE INDEX IF NOT EXISTS "atu_LC_samples_antenna" ON  "atu_LC_samples" (antenna, frequency)');

    databaseTransaction.Commit;
  end;
end;

procedure TSpertDatabase.Clear;
begin
  databaseTransaction.StartTransaction;
  databaseConnection.ExecuteDirect('DELETE FROM swr_samples');
  databaseConnection.ExecuteDirect('DELETE FROM atu_LC_samples');
  databaseTransaction.Commit;
end;

procedure TSpertDatabase.insertAtuLC(frequency: Longword; L, C: Double; K: Byte);
var
  Frq: Longword;
  Sql: String;
  rCount: Integer;
begin
  if active and (frequency > 0) and ((L > 0) or (C > 0)) then
  begin
    Frq:=Trunc(frequency / 1000) * 1000;
    databaseTransaction.StartTransaction;
    databaseQuery.SQL.Text:='SELECT COUNT(*) FROM atu_LC_samples WHERE antenna = "' + Configuration.Settings.spertAntenna + '" AND frequency = ' + IntToStr(Frq);
    databaseQuery.Open;
    rCount:=databaseQuery.Fields[0].AsInteger;
    databaseQuery.Close;
    if rCount = 0
      then Sql:='INSERT INTO atu_LC_samples (antenna, frequency, L, C, K, ts) VALUES("' + Configuration.Settings.spertAntenna + '", ' + IntToStr(Frq)  + ', ' + FloatToStr(L) + ', ' + FloatToStr(C) + ', ' + IntToStr(K) + ', ' + IntToStr(DateTimeToUnix(Now())) + ')'
      else Sql:='UPDATE atu_LC_samples SET L = ' + FloatToStr(L)  + ', C = '+ FloatToStr(C) +', K = '+ IntToStr(K) +', ts = ' + IntToStr(DateTimeToUnix(Now())) + ' WHERE antenna = "' + Configuration.Settings.spertAntenna + '" AND frequency = ' + IntToStr(Frq);
    FormDebug.Log('[Spert] ' + Sql);
    databaseConnection.ExecuteDirect(sql);
    databaseTransaction.Commit;
  end;
end;

function TSpertDatabase.matchLC(frequency: Longword; span: Integer; L, C: Double): TSpertDatabaseLCmatch;
var
  Frq: Longword;
  cnt: Integer = 0;
  sumL: Double = 0;
  sumC: Double = 0;
  avgL: Double;
  avgC: Double;
begin
  Result:=SpertDatabaseLC_Unknown;

  if active and (frequency > 0) then
  begin
    Frq:=Trunc(frequency / 1000) * 1000;
    databaseTransaction.StartTransaction;
    databaseQuery.SQL.Text:=Format('SELECT L, C FROM atu_LC_samples WHERE antenna = "%s" AND (frequency BETWEEN %d AND %d)', [Configuration.Settings.spertAntenna, (Frq - span), (Frq + span)]);
    databaseQuery.Open;
    while not databaseQuery.EOF do begin
      sumL:=sumL + databaseQuery.Fields[0].AsFloat;
      sumC:=sumC + databaseQuery.Fields[1].AsFloat;
      Inc(cnt);
      databaseQuery.Next;
    end;
    databaseQuery.Close;
    if cnt > 0 then
    begin
      avgL:=(sumL / cnt);
      avgC:=(sumC / cnt);
      if (L <= (avgL * 2)) and (L >= (avgL / 2)) and (C <= (avgC * 2)) and (C >= (avgC / 2)) // this relates to TSpert.ATU_L/ATU_C - previous L/C is 2x smaller then current, next 2x bigger
        then Result:=SpertDatabaseLC_Match
        else Result:=SpertDatabaseLC_NotMatch;
    end;
    databaseTransaction.Commit;
  end;
end;

procedure TSpertDatabase.insertSwr(frequency: Longword; swr: Double);
var
  Frq: Longword;
  Sql: String;
  rCount: Integer;
begin
  if active and (frequency > 0) then
  begin
    Frq:=Trunc(frequency / 1000) * 1000;
    databaseTransaction.StartTransaction;
    databaseQuery.SQL.Text:='SELECT COUNT(*) FROM swr_samples WHERE antenna = "' + Configuration.Settings.spertAntenna + '" AND frequency = ' + IntToStr(Frq);
    databaseQuery.Open;
    rCount:=databaseQuery.Fields[0].AsInteger;
    databaseQuery.Close;
    if rCount = 0
      then Sql:='INSERT INTO swr_samples (antenna, frequency, swr, ts) VALUES("' + Configuration.Settings.spertAntenna + '", ' + IntToStr(Frq)  + ', ' + FloatToStr(swr) + ', ' + IntToStr(DateTimeToUnix(Now())) + ')'
      else Sql:='UPDATE swr_samples SET swr = ' + FloatToStr(swr)  + ', ts = ' + IntToStr(DateTimeToUnix(Now())) + ' WHERE antenna = "' + Configuration.Settings.spertAntenna + '" AND frequency = ' + IntToStr(Frq);
    FormDebug.Log('[Spert] ' + Sql);
    databaseConnection.ExecuteDirect(sql);
    databaseTransaction.Commit;
  end;
end;

function TSpertDatabase.estimateSwr(frequency: Longword; span: Integer): Double;
var
  Frq: Longword;
  Swr: String;
begin
  if active then
  begin
    Frq:=Trunc(frequency / 1000) * 1000;
    databaseTransaction.StartTransaction;
    databaseQuery.SQL.Text:='SELECT AVG(swr) FROM swr_samples WHERE antenna = "' + Configuration.Settings.spertAntenna + '" AND frequency BETWEEN ' + IntToStr(Frq - span) + ' AND ' + IntToStr(Frq + span);
    databaseQuery.Open;
    Swr:=databaseQuery.Fields[0].AsString;
    databaseQuery.Close;
    if Swr <> '' then Result:=StrToFloat(Swr) else Result:=-1;
    databaseTransaction.Commit;
  end;
end;

end.

