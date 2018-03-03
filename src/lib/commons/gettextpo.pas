unit gettextpo;

interface
uses
  SysUtils, Classes, TStringUnit;

type
  TPOFile = class(TObject)
  private
    FValues: TStringList;
  protected
    
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetMsg(const sMsgId, sMsgVal : ansistring);
    procedure RemoveMsg(const sMsgId : ansistring);

    function GetMsg(const sMsgId : ansistring) : ansistring;
    function HasMsg(const sMsgId: ansistring): integer;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    procedure Clear();
  end;

implementation

{ TPOFile }

procedure TPOFile.Clear;
var
  ACount : Integer;
begin
  for ACount := 0 to FValues.Count-1 do
    FValues.Objects[ACount].Free;
  FValues.Clear;
end;

constructor TPOFile.Create;
begin
  inherited Create;
  FValues := TStringList.Create;
  FValues.Duplicates := dupIgnore;
  FValues.Sorted := True;
end;

destructor TPOFile.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

function TPOFile.GetMsg(const sMsgId: ansistring): ansistring;
var
  AIndex: Integer;
begin
  Result := sMsgId;
  AIndex := HasMsg(sMsgId);
  if AIndex >= 0 then
    Result := TString(FValues.Objects[AIndex]).GetString;
end;

function TPOFile.HasMsg(const sMsgId: ansistring): integer;
begin
  Result := FValues.IndexOf(sMsgId);
end;

procedure TPOFile.LoadFromStream(Stream: TStream);

  function GetString(AString: TStrings; var ACount: integer): String;
  begin
    Result := EmptyStr;
    if ACount < AString.Count then
    begin
      while (Result = EmptyStr) or (ACount < AString.Count) do
      begin
        if (Length(AString[ACount]) > 0) and
           (AString[ACount][1] <> '#') then
          Result := AString[ACount];        
        Inc(ACount);
      end;
      Result := AString[ACount];
      Inc(ACount);
    end;
  end;

var
  AStrings : TStrings;
  ACount : Integer;
  AIndex : Integer;
  AStat: Byte;
  AStr, ABuffer, AMsg: String;
  AMsgId: String;
  AMsgStr: String;
begin
  Clear;
  AStrings := TStringList.Create;
  try
    AStrings.LoadFromStream(Stream);
    ACount := 0;
    AStat := 0;
    while ACount < AStrings.Count do
    begin
      AStr := GetString(AStrings, ACount);
      if Pos('msgid ', AStr) = 1 then
      begin
        if AStat = 0 then
          AStat := 1
        else
        begin
          AStat := 0;
          AMsgStr := ABuffer;        
        end;
        ABuffer := Copy(AStr, 7, Length(AStr)-6);
      end else
      if Pos('msgstr ', AStr) = 1 then
      begin
        if AStat = 1 then
        begin
          AStat := 2;
          AMsgId := ABuffer;
        end;
        ABuffer := Copy(AStr, 7, Length(AStr)-6);
      end else
        ABuffer := ABuffer + #13#10 + AStr;
      if (AStat = 0) and ((AMsgId <> '""') or (AMsgStr <> '""')) then
      begin
        SetMsg(AMsgId, AMsgStr);
        AMsgId := EmptyStr;
        AMsgStr := EmptyStr;
      end;
    end;
  finally
    AStrings.Free;
  end;
end;

procedure TPOFile.RemoveMsg(const sMsgId: ansistring);
var
  AIndex: Integer;
begin
  AIndex := HasMsg(sMsgId);
  if AIndex >= 0 then
  begin
    FValues.Objects[AIndex].Free;
    FValues.Delete(AIndex);
  end;
end;

procedure TPOFile.SaveToStream(Stream: TStream);
var
  AStrings: TStringList;
  I: integer;
begin
  AStrings := TStringList.Create;
  try
    for I := 0 to FValues.Count - 1 do
    begin
      AStrings.Add(FValues[I])
    end;      
  finally
    AStrings.Free;
  end;
end;

procedure TPOFile.SetMsg(const sMsgId, sMsgVal: ansistring);
begin
  FValues.AddObject(Trim(sMsgId), TString.Create(Trim(sMsgVal)));
end;

end.
