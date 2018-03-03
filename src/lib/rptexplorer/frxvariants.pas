unit frxvariants;

interface

uses
  SysUtils, Classes, fs_iinterpreter;

implementation

uses Variants;

type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TFunctions }

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'VARISARRAY' then
    Result := VarIsArray(Caller.Params[0])
  else if MethodName = 'VARARRAYDIMCOUNT' then
    Result := VarArrayDimCount(Caller.Params[0])
  else if MethodName = 'VARARRAYLOWBOUND' then
    Result := VarArrayLowBound(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'VARARRAYHIGHBOUND' then
    Result := VarArrayHighBound(Caller.Params[0], Caller.Params[1]);    
end;

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    AddMethod('function VarIsArray(const A: Variant): Boolean;', CallMethod,'Variant Functions',
      'Indicates whether the specified variant is an array.');
    AddMethod('function VarArrayDimCount(const A: Variant): Integer;', CallMethod,'Variant Functions',
      'Returns number of dimensions of a variant array.');
    AddMethod('function VarArrayLowBound(const A: Variant; Dim: Integer): Integer;', CallMethod,'Variant Functions',
      'Returns the low bound of a dimension in a variant array.');
    AddMethod('function VarArrayHighBound(const A: Variant; Dim: Integer): Integer;', CallMethod,'Variant Functions',
      'Returns high bound for a dimension in a variant array.');
  end;
end;

initialization
  fsRTTIModules.Add(TFunctions);
end.

