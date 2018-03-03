unit frxECFRTTI;

interface

{$I frx.inc}

implementation

uses
  Windows, Classes, SysUtils, Forms, fs_iinterpreter, frxECFComponents,
  fs_iecfrtti
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
    with AddClass(TfrxECFAPIObject, 'TfrxCustomDataset') do
    begin
      AddMethod('function ParamByName(Name: string): TfrxParamItem', CallMethod);
      AddMethod('procedure prepare', CallMethod);
      AddMethod('procedure ExecInsert', CallMethod);
      AddMethod('procedure ExecUpdate', CallMethod);
      AddMethod('procedure ExecDelete', CallMethod);
      AddProperty('APIObject', 'TfrxECFAPIObject', GetProp, nil);
    end;
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  Result := 0;
  if ClassType = TfrxECFAPIObject then
  begin
    if MethodName = 'EXECINSERT' then
      TfrxECFAPIObject(Instance).ExecInsert
    else if MethodName = 'EXECUPDATE' then
      TfrxECFAPIObject(Instance).ExecUpdate
    else if MethodName = 'EXECDELETE' then
      TfrxECFAPIObject(Instance).ExecDelete
    else if MethodName = 'PREPARE' then
      TfrxECFAPIObject(Instance).Prepare
    else if MethodName = 'PARAMBYNAME' then
      Result := Integer(TfrxECFAPIObject(Instance).ParamByName(Caller.Params[0]))
  end
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;
  if ClassType = TfrxECFAPIObject then
  begin
    if PropName = 'APIOBJECT' then
      Result := Integer(TfrxECFAPIObject(Instance).APIObject)
  end
end;


initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);

end.
