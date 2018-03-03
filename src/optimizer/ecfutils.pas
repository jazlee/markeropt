unit ecfutils;

interface

uses
  SysUtils, Classes, DB;

type
  TECFParam = class(TParam)
  private
    FCaption: string;
    FCategory: string;
    FInheritable: boolean;

  protected
    procedure AssignECFParam(Param: TECFParam);

  public
    procedure Assign(Source: TPersistent); override;

  published
    property Caption: string read FCaption write FCaption;
    property Category: string read FCategory write FCategory;
    property Inheritable: boolean read FInheritable write FInheritable;
  end;

  TECFParams = class(TParams)
  private
    FOwner: TPersistent;
    function GetParamCaption(const ParamName: string): Variant;
    procedure SetParamCaption(const ParamName: string; const Value: Variant);
    function GetParamCategory(const ParamName: string): Variant;
    procedure SetParamCategory(const ParamName: string; const Value: Variant);
    function GetParamInheritables(const ParamName: string): Variant;
    procedure SetParamInheritables(const ParamName: string;
      const Value: Variant);
  protected

    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);

    procedure ParseParams(const AParams: String);
    procedure AssignInheritable(Source: TPersistent);



    property ParamCaptions[const ParamName: string]: Variant
      read GetParamCaption write SetParamCaption;
    property ParamCategories[const ParamName: string]: Variant
      read GetParamCategory write SetParamCategory;
    property ParamInheritables[const ParamName: string]: Variant
      read GetParamInheritables write SetParamInheritables;
  end;

procedure ExtractXMLProps(const PropStr: string; var Pos: Integer; var Name, Value: string);
function ExtractFieldName(const Fields: string; var Pos: Integer): string;
function FieldCount(const Fields: string): integer;
function DateToInt(ADate: TDateTime): integer;
function TimeToInt(ATime: TDateTime): integer;
function StripDot(Const AStr: string): string;  

implementation
uses
  Variants;

procedure ExtractXMLProps(const PropStr: string; var Pos: Integer; var Name, Value: string);
var
  ALen, J, AStart: Integer;
  ALen1: Integer;
  AChar, AChar1: PChar;
begin
  Name := '';
  Value:= '';
  ALen := PCardinal(@PChar(PropStr)[-4])^;
  AChar:= PChar(PropStr)-1;
  J := Pos;
  ALen1 := ALen;
  AChar1 := AChar;
  while (J < ALen1) and (AChar1[J] = ' ') do
    Inc(J);
  AStart := J;
  while (J < ALen1) and (AChar1[J] <> '=') do
    Inc(J);
  Pos := J;
  if Pos < ALen then
  begin
    J := Pos - 1;
    while (J > 0) and (AChar1[j] = ' ') do
      Dec(J);
    Name := Copy(PropStr, AStart, J - AStart + 1);
    if Name = '' then
      Exit;
    J := Pos;
    ALen1 := ALen;
    while (J < ALen1) and (AChar1[J] <> '"') do
      Inc(J);
    AStart := J + 1;
    Inc(J);
    while (J < ALen1) and (AChar1[J] <> '"') do
      Inc(J);
    Pos := j;
    Value := Copy(PropStr, AStart, Pos - AStart);
    Inc(Pos);      
  end;   
end;

function DateToInt(ADate: TDateTime): integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  Result := (((AYear * 100)+AMonth) * 100)+ADay;
end;

function TimeToInt(ATime: TDateTime): integer;
var
  AHour, AMinute, ASec, AMSec: Word;
begin
  DecodeTime(ATime, AHour, AMinute, ASec, AMSec);
  Result := (((AHour * 100) + AMinute) * 100)+ASec;
end;

function StripDot(Const AStr: string): string;
var
  I: integer;
begin
  Result := EmptyStr;
  for I := 1 to Length(AStr) do
    if AStr[I] <> '.' then
      Result := Result + AStr[I];    
end;

function ExtractFieldName(const Fields: string; var Pos: Integer): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(Fields)) and (Fields[I] <> ';') do Inc(I);
  Result := Trim(Copy(Fields, Pos, I - Pos));
  if (I <= Length(Fields)) and (Fields[I] = ';') then Inc(I);
  Pos := I;
end;

function FieldCount(const Fields: string): integer;
var
  nPos: integer;
begin
  nPos:= 1;
  Result := 0;
  while nPos <= Length(Fields) do
  begin
    ExtractFieldName(Fields, nPos);
    inc(Result);
  end;
end;

{ TECFParams }

procedure TECFParams.AssignInheritable(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TECFParams then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TECFParams(Source).Count - 1 do
        if TECFParam(TECFParams(Source).Items[I]).Inheritable then        
          Add.Assign(TCollection(Source).Items[I]);
    finally
      EndUpdate;
    end;
  end;
end;

constructor TECFParams.Create(Owner: TPersistent);
begin
  Self.FOwner := Owner;
  inherited Create(TECFParam);
end;

function TECFParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TECFParams.GetParamCaption(const ParamName: string): Variant;
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      Result := VarArrayCreate([0, Params.Count - 1], varVariant);
      for I := 0 to Params.Count - 1 do
        Result[I] := TECFParam(Params[I]).Caption;
    finally
      Params.Free;
    end;
  end else
  if SameText('*', ParamName) then
  begin
    Result := VarArrayCreate([0, Count - 1], varVariant);
    for I := 0 to Count - 1 do
      Result[I] := TECFParam(Items[I]).Caption;
  end else  
    Result := TECFParam(ParamByName(ParamName)).Caption
end;

function TECFParams.GetParamCategory(const ParamName: string): Variant;
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      Result := VarArrayCreate([0, Params.Count - 1], varVariant);
      for I := 0 to Params.Count - 1 do
        Result[I] := TECFParam(Params[I]).Category;
    finally
      Params.Free;
    end;
  end else
  if SameText('*', ParamName) then
  begin
    Result := VarArrayCreate([0, Count - 1], varVariant);
    for I := 0 to Count - 1 do
      Result[I] := TECFParam(Items[I]).Category;
  end else
    Result := TECFParam(ParamByName(ParamName)).Category
end;

function TECFParams.GetParamInheritables(const ParamName: string): Variant;
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      Result := VarArrayCreate([0, Params.Count - 1], varVariant);
      for I := 0 to Params.Count - 1 do
        Result[I] := TECFParam(Params[I]).Inheritable;
    finally
      Params.Free;
    end;
  end else
  if SameText('*', ParamName) then
  begin
    Result := VarArrayCreate([0, Count - 1], varVariant);
    for I := 0 to Count - 1 do
      Result[I] := TECFParam(Items[I]).Inheritable;
  end else  
    Result := TECFParam(ParamByName(ParamName)).Inheritable;
end;

procedure TECFParams.ParseParams(const AParams: String);
var
  APos: integer;
  AParam, ASQL: String;
begin
  APos := 1;
  ASQL := '';
  while APos <= Length(AParams) do
  begin
    AParam := ExtractFieldName(AParams, APos);
    ASQL := ASQL + Format(':%s ',[AParam]);    
  end;
  ParseSQL(ASQL, True); 
end;

procedure TECFParams.SetParamCaption(const ParamName: string;
  const Value: Variant);
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      for I := 0 to Params.Count - 1 do
        TECFParam(Params[I]).Caption := Value[I];
    finally
      Params.Free;
    end;
  end else
  if SameText('*', ParamName) then
  begin
    for I := 0 to Count - 1 do
      TECFParam(Items[I]).Caption := Value[I];
  end else  
    TECFParam(ParamByName(ParamName)).Caption := Value;
end;

procedure TECFParams.SetParamCategory(const ParamName: string;
  const Value: Variant);
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      for I := 0 to Params.Count - 1 do
        TECFParam(Params[I]).Category := Value[I];
    finally
      Params.Free;
    end;
  end else
  if SameText('*', ParamName) then
  begin
    for I := 0 to Count - 1 do
      TECFParam(Items[I]).Category := Value[I];
  end else  
    TECFParam(ParamByName(ParamName)).Category := Value;
end;

procedure TECFParams.SetParamInheritables(const ParamName: string;
  const Value: Variant);
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      for I := 0 to Params.Count - 1 do
        TECFParam(Params[I]).Inheritable := Value[I];
    finally
      Params.Free;
    end;
  end else
  if SameText('*', ParamName) then
  begin
    for I := 0 to Count - 1 do
      TECFParam(Items[I]).Inheritable := Value[I];
  end else  
    TECFParam(ParamByName(ParamName)).Inheritable := Value;
end;

{ TECFParam }

procedure TECFParam.Assign(Source: TPersistent);
begin
  if Source is TECFParam then
    AssignECFParam(TECFParam(Source))
  else
    inherited Assign(Source);
end;

procedure TECFParam.AssignECFParam(Param: TECFParam);
begin
  AssignParam(Param);
  if Param <> nil then
  begin
    FCaption := Param.Caption;
    FCategory:= Param.Category;
    FInheritable := Param.Inheritable;
  end;  
end;

end.
