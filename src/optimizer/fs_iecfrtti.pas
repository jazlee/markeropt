unit fs_iecfrtti;

interface

{$i fs.inc}

uses
  SysUtils, Classes, fs_iinterpreter, fs_itools, fs_idbrtti,
  DB, frxECFComponents;

type
  TfsEcfRTTI = class(TComponent); // fake component  

implementation

uses
  kbmMWCustomDataset, ecfdataset, ecfapiproc;
  
type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
    procedure SetProp(Instance: TObject; ClassType: TClass;
      const PropName: String; Value: Variant);
  public
    constructor Create(AScript: TfsScript); override;
  end;


{ TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    with AddClass(TECFCustomAPIPooledCursor, 'TDataset') do
    begin
      AddMethod('function FindKey(const KeyValues: array): Boolean', CallMethod);
      AddMethod('procedure FindNearest(const KeyValues: array)', CallMethod);
      AddMethod('procedure Prepare', CallMethod);
      AddMethod('procedure ExecInsert', CallMethod);
      AddMethod('procedure ExecUpdate', CallMethod);
      AddMethod('procedure ExecDelete', CallMethod);
      AddMethod('function ParamByName(const Value: string): TParam', CallMethod);
      AddProperty('ParamCount', 'Word', GetProp, nil);
    end;
    with AddClass(TECFAPIObject, 'TECFCustomAPIPooledCursor') do
    begin
      AddProperty('APIObjectName', 'String', GetProp, SetProp);
      AddProperty('APIMethodName', 'String', GetProp, SetProp);
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
    Result := TECFCustomAPIPooledCursor(Instance).FindKey(ar);
    ClearVarRec(ar);
  end;

  procedure DoFindNearest;
  var
    ar: TVarRecArray;
  begin
    VariantToVarRec(Caller.Params[0], ar);
    TECFCustomAPIPooledCursor(Instance).FindNearest(ar);
    ClearVarRec(ar);
  end;

begin
  Result := 0;
  if ClassType = TECFCustomAPIPooledCursor then
  begin
    if MethodName = 'FINDKEY' then
      Result := DoFindKey
    else if MethodName = 'FINDNEAREST' then
      DoFindNearest
    else if MethodName = 'EXECINSERT' then
      TECFCustomAPIPooledCursor(Instance).ExecInsert
    else if MethodName = 'EXECUPDATE' then
      TECFCustomAPIPooledCursor(Instance).ExecUpdate
    else if MethodName = 'EXECDELETE' then
      TECFCustomAPIPooledCursor(Instance).ExecDelete
    else if MethodName = 'EXECDELETE' then
      TECFCustomAPIPooledCursor(Instance).ExecDelete
    else if MethodName = 'PARAMBYNAME' then
      Result := Integer(TECFCustomAPIPooledCursor(Instance).ParamByName[Caller.Params[0]])
  end
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;
  if ClassType = TECFCustomAPIPooledCursor then
  begin
    if PropName = 'PARAMCOUNT' then
      Result := TECFCustomAPIPooledCursor(Instance).Params.Count
  end
  else if ClassType = TECFAPIObject then
  begin
    if PropName = 'APIOBJECTNAME' then
      Result := TECFAPIObject(Instance).APIObjectName
    else if PropName = 'APIMETHODNAME' then
      Result := TECFAPIObject(Instance).APIMethodName
  end
end;

procedure TFunctions.SetProp(Instance: TObject; ClassType: TClass;
  const PropName: String; Value: Variant);
begin
  if ClassType = TECFAPIObject then
  begin
    if PropName = 'APIOBJECTNAME' then
      TECFAPIObject(Instance).APIObjectName := Value
    else if PropName = 'APIMETHODNAME' then
      TECFAPIObject(Instance).APIMethodName := Value
  end
end;

initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);
end.

