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

{ Locale-safe Double to SQL literal. FPC's FloatToStr honors FormatSettings.DecimalSeparator,
  which under pl_PL locale produces "1,5" — SQLite parses that as two separate columns and
  the INSERT/UPDATE silently fails. Force '.' regardless of system locale. }
function floatSql(x: Double): String;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;
  Result := FloatToStr(x, fs);
end;

{ SQL single-quoted string literal with embedded apostrophes doubled per SQL standard.
  Replaces the previous double-quoted form which SQLite treats as identifier-with-fallback. }
function sqlStr(s: String): String;
begin
  Result := '''' + StringReplace(s, '''', '''''', [rfReplaceAll]) + '''';
end;

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
  ant, ts: String;
  Sql: String;
  rCount: Integer;
begin
  if active and (frequency > 0) and ((L > 0) or (C > 0)) then
  begin
    Frq:=Trunc(frequency / 1000) * 1000;
    ant:=sqlStr(Configuration.Settings.spertAntenna);
    ts:=IntToStr(DateTimeToUnix(Now()));
    databaseTransaction.StartTransaction;
    databaseQuery.SQL.Text:='SELECT COUNT(*) FROM atu_LC_samples WHERE antenna = ' + ant + ' AND frequency = ' + IntToStr(Frq);
    databaseQuery.Open;
    rCount:=databaseQuery.Fields[0].AsInteger;
    databaseQuery.Close;
    if rCount = 0
      then Sql:='INSERT INTO atu_LC_samples (antenna, frequency, L, C, K, ts) VALUES(' + ant + ', ' + IntToStr(Frq) + ', ' + floatSql(L) + ', ' + floatSql(C) + ', ' + IntToStr(K) + ', ' + ts + ')'
      else Sql:='UPDATE atu_LC_samples SET L = ' + floatSql(L) + ', C = ' + floatSql(C) + ', K = ' + IntToStr(K) + ', ts = ' + ts + ' WHERE antenna = ' + ant + ' AND frequency = ' + IntToStr(Frq);
    FormDebug.Log('[Spert] ' + Sql);
    databaseConnection.ExecuteDirect(sql);
    databaseTransaction.Commit;
  end;
end;

function TSpertDatabase.matchLC(frequency: Longword; span: Integer; L, C: Double): TSpertDatabaseLCmatch;
const
  { L is in µH, smallest bank value 0.04. Epsilon below that but well above IEEE 754
    round-trip noise (~1e-16) — catches a real bit-flip, ignores float precision drift. }
  L_EPSILON = 0.001;
  { C is in pF, smallest bank value 5. Same logic. }
  C_EPSILON = 0.1;
var
  Frq: Longword;
  savedL, savedC: Double;
begin
  Result:=SpertDatabaseLC_Unknown;

  if active and (frequency > 0) then
  begin
    Frq:=Trunc(frequency / 1000) * 1000;
    databaseTransaction.StartTransaction;
    { Pick the closest stored sample by frequency within span — antenna L/C varies
      smoothly with frequency, so the nearest tuned point is the authoritative
      reference for the current operating frequency. }
    databaseQuery.SQL.Text:='SELECT L, C FROM atu_LC_samples WHERE antenna = ' + sqlStr(Configuration.Settings.spertAntenna) +
      ' AND frequency BETWEEN ' + IntToStr(Frq - span) + ' AND ' + IntToStr(Frq + span) +
      ' ORDER BY ABS(frequency - ' + IntToStr(Frq) + ') ASC LIMIT 1';
    databaseQuery.Open;
    if not databaseQuery.EOF then
    begin
      savedL := databaseQuery.Fields[0].AsFloat;
      savedC := databaseQuery.Fields[1].AsFloat;
      { Exact match: identical bit patterns from calcAtu_LC produce identical Doubles,
        and SQLite round-trip preserves them. Epsilon is precision-noise insurance,
        not a tolerance window — a one-bit ATU change must register as NotMatch. }
      if (Abs(L - savedL) < L_EPSILON) and (Abs(C - savedC) < C_EPSILON)
        then Result:=SpertDatabaseLC_Match
        else Result:=SpertDatabaseLC_NotMatch;
    end;
    databaseQuery.Close;
    databaseTransaction.Commit;
  end;
end;

procedure TSpertDatabase.insertSwr(frequency: Longword; swr: Double);
var
  Frq: Longword;
  ant, ts: String;
  Sql: String;
  rCount: Integer;
begin
  { swr >= 0 guards against bogus negative readings; the caller is expected to
    skip near-zero TX sessions (no real measurement) before reaching this point. }
  if active and (frequency > 0) and (swr >= 0) then
  begin
    Frq:=Trunc(frequency / 1000) * 1000;
    ant:=sqlStr(Configuration.Settings.spertAntenna);
    ts:=IntToStr(DateTimeToUnix(Now()));
    databaseTransaction.StartTransaction;
    databaseQuery.SQL.Text:='SELECT COUNT(*) FROM swr_samples WHERE antenna = ' + ant + ' AND frequency = ' + IntToStr(Frq);
    databaseQuery.Open;
    rCount:=databaseQuery.Fields[0].AsInteger;
    databaseQuery.Close;
    if rCount = 0
      then Sql:='INSERT INTO swr_samples (antenna, frequency, swr, ts) VALUES(' + ant + ', ' + IntToStr(Frq) + ', ' + floatSql(swr) + ', ' + ts + ')'
      else Sql:='UPDATE swr_samples SET swr = ' + floatSql(swr) + ', ts = ' + ts + ' WHERE antenna = ' + ant + ' AND frequency = ' + IntToStr(Frq);
    FormDebug.Log('[Spert] ' + Sql);
    databaseConnection.ExecuteDirect(sql);
    databaseTransaction.Commit;
  end;
end;

function TSpertDatabase.estimateSwr(frequency: Longword; span: Integer): Double;
var
  Frq: Longword;
begin
  Result := -1;
  if active then
  begin
    Frq:=Trunc(frequency / 1000) * 1000;
    databaseTransaction.StartTransaction;
    databaseQuery.SQL.Text:='SELECT AVG(swr) FROM swr_samples WHERE antenna = ' + sqlStr(Configuration.Settings.spertAntenna) + ' AND frequency BETWEEN ' + IntToStr(Frq - span) + ' AND ' + IntToStr(Frq + span);
    databaseQuery.Open;
    { AVG returns NULL when no rows match → IsNull check distinguishes that from a real 0.0 average.
      AsFloat goes through the SQLite native API and is locale-independent (unlike AsString + StrToFloat). }
    if not databaseQuery.Fields[0].IsNull then
      Result := databaseQuery.Fields[0].AsFloat;
    databaseQuery.Close;
    databaseTransaction.Commit;
  end;
end;

end.

