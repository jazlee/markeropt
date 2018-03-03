unit fs_iacrrtti;

interface

{$i fs.inc}

uses
  SysUtils, Classes, fs_iinterpreter, fs_itools, fs_idbrtti,
  DB, ACRMain;

type
  TfsACRRTTI = class(TComponent); // fake component


implementation

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
    AddEnum('TTableType', 'ttDefault, ttParadox, ttDBase, ttFoxPro, ttASCII');
    AddEnum('TParamBindMode', 'pbByName, pbByNumber');

    AddClass(TACRSession, 'TComponent');
    AddClass(TACRDatabase, 'TComponent');
    AddClass(TACRDataSet, 'TDataSet');
    with AddClass(TACRTable, 'TACRDataSet') do
    begin
      AddMethod('procedure CreateTable', CallMethod);
      AddMethod('procedure DeleteTable', CallMethod);
      AddMethod('procedure EmptyTable', CallMethod);
      AddMethod('function FindKey(const KeyValues: array): Boolean', CallMethod);
      AddMethod('procedure FindNearest(const KeyValues: array)', CallMethod);
      AddMethod('procedure RenameTable(const NewTableName: string)', CallMethod);
    end;
    with AddClass(TACRQuery, 'TACRDataSet') do
    begin
      AddMethod('procedure ExecSQL', CallMethod);
      AddMethod('function ParamByName(const Value: string): TParam', CallMethod);
      AddMethod('procedure Prepare', CallMethod);
      AddProperty('ParamCount', 'Word', GetProp, nil);
    end;
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;

  function DoFindKey: Boolean;
  var
    ar: TVarRecArray;
  begin
    VariantToVarRec(Caller.Params[0], ar);
    Result := TACRTable(Instance).FindKey(ar);
    ClearVarRec(ar);
  end;

  procedure DoFindNearest;
  var
    ar: TVarRecArray;
  begin
    VariantToVarRec(Caller.Params[0], ar);
    TACRTable(Instance).FindNearest(ar);
    ClearVarRec(ar);
  end;

begin
  Result := 0;

  if ClassType = TACRTable then
  begin
    if MethodName = 'CREATETABLE' then
      TACRTable(Instance).CreateTable
    else if MethodName = 'DELETETABLE' then
      TACRTable(Instance).DeleteTable
    else if MethodName = 'EMPTYTABLE' then
      TACRTable(Instance).EmptyTable
    else if MethodName = 'FINDKEY' then
      Result := DoFindKey
    else if MethodName = 'FINDNEAREST' then
      DoFindNearest
    else if MethodName = 'RENAMETABLE' then
      TACRTable(Instance).RenameTable(Caller.Params[0])
  end
  else if ClassType = TACRQuery then
  begin
    if MethodName = 'EXECSQL' then
      TACRQuery(Instance).ExecSQL
    else if MethodName = 'PARAMBYNAME' then
      Result := Integer(TACRQuery(Instance).ParamByName(Caller.Params[0]))
    else if MethodName = 'PREPARE' then
      TACRQuery(Instance).Prepare
  end
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;

  if ClassType = TACRQuery then
  begin
    if PropName = 'PARAMCOUNT' then
      Result := TACRQuery(Instance).ParamCount
  end
end;


initialization
  fsRTTIModules.Add(TFunctions);

finalization
  fsRTTIModules.Remove(TFunctions);

end.

