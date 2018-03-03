unit frxACRRTTI;

interface

{$I frx.inc}

implementation

uses
  Windows, Classes, SysUtils, Forms, fs_iinterpreter, frxACRComponents,
  fs_iacrrtti
{$IFDEF Delphi6}
, Variants
{$ENDIF};
  

type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;


{ TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    with AddClass(TfrxACRDatabase, 'TfrxCustomDatabase') do
      AddProperty('Database', 'TDatabase', GetProp, nil);
    with AddClass(TfrxACRTable, 'TfrxCustomTable') do
      AddProperty('Table', 'TTable', GetProp, nil);
    with AddClass(TfrxACRQuery, 'TfrxCustomQuery') do
    begin
      AddMethod('procedure ExecSQL', CallMethod);
      AddProperty('Query', 'TQuery', GetProp, nil);
    end;
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  Result := 0;

  if ClassType = TfrxACRQuery then
  begin
    if MethodName = 'EXECSQL' then
      TfrxACRQuery(Instance).Query.ExecSQL
  end
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;

  if ClassType = TfrxACRDatabase then
  begin
    if PropName = 'DATABASE' then
      Result := Integer(TfrxACRDatabase(Instance).Database)
  end
  else if ClassType = TfrxACRTable then
  begin
    if PropName = 'TABLE' then
      Result := Integer(TfrxACRTable(Instance).Table)
  end
  else if ClassType = TfrxACRQuery then
  begin
    if PropName = 'QUERY' then
      Result := Integer(TfrxACRQuery(Instance).Query)
  end
end;


initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);

end.

