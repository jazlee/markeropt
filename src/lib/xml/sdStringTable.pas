unit sdStringTable;

interface

uses
  Classes, SysUtils, Contnrs;

type

  // A record describing a string by its first position and length (Count)
  TsdStringRec = record
    First: Pbyte;
    Count: integer;
  end;

  // A string reference item used in string reference lists (do not use directly)
  TsdRefString = class
  private
    FStringID: integer;
    FRefCount: integer;
    FFirst: Pbyte;
    FCount: integer;
  protected
    procedure SetString(const SR: TsdStringRec);
    function CompareToSR(const SR: TsdStringRec): integer;
    function StringRec: TsdStringRec;
    function AsString: string;
  public
    destructor Destroy; override;
  end;

  // A list of string reference items (do not use directly)
  TsdRefStringList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdRefString;
  protected
    // Assumes list is sorted by StringID
    function IndexOfID(AStringID: integer; var Index: integer): boolean;
    // Assumes list is sorted by string rec
    function IndexOfSR(const AStringRec: TsdStringRec; var Index: integer): boolean;
  public
    property Items[Index: integer]: TsdRefString read GetItems; default;
  end;

  // A string table, holding a collection of unique strings, sorted in 2 ways
  // for fast access. Strings can be added with AddString or AddStringRec,
  // and should be updated with SetString. When a string is added or updated,
  // an ID is returned which the application can use to retrieve the string,
  // using GetString.
  TsdStringTable = class(TPersistent)
  private
    FByID: TsdRefStringList;
    FBySR: TsdRefStringList;
  protected
    procedure DecRefCount(AItem: TsdRefString; ByIdIndex: integer);
    function NextUniqueID: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    // Add a new string rec, return fresh ID or ID of existing item, and increase
    // the existing item's ref count
    function AddStringRec(const SR: TsdStringRec): integer;
    // Add a new string S to the table, the function returns its ID.
    function AddString(const S: string): integer;
    // Get the string of refstring with ID
    function GetString(ID: integer): string;
    // Set the string value of refstring with ID.
    procedure SetString(var ID: integer; const S: string);
    function StringCount: integer;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(S: TStream);
  end;

// convert a string into a string rec
function sdStringToSR(const S: string): TsdStringRec;
// convert a string rec into a string
function sdSRToString(const SR: TsdStringRec): string;
// compare two string recs. This is NOT an alphabetic compare. SRs are first
// compared by length, then by first byte, then last byte then second, then
// N-1, until all bytes are compared.
function sdCompareSR(const SR1,  SR2: TsdStringRec): integer;


implementation

function sdStringToSR(const S: string): TsdStringRec;
begin
  Result.Count := length(S);
  if Result.Count = 0 then
    Result.First := nil
  else
    Result.First := @S[1];
end;

function sdSRToString(const SR: TsdStringRec): string;
begin
  SetLength(Result, SR.Count);
  if SR.Count > 0 then
    Move(SR.First^, Result[1], SR.Count);
end;

function CompareInteger(Int1, Int2: integer): integer;
begin
  if Int1 < Int2 then Result := -1 else
    if Int1 > Int2 then Result := 1 else
      Result := 0;
end;

function sdCompareSR(const SR1,  SR2: TsdStringRec): integer;
var
  Count: integer;
  First1, First2, Last1, Last2: Pbyte;
begin
  // Compare string length first
  Result := CompareInteger(SR1.Count, SR2.Count);
  if Result <> 0 then exit;
  // Compare first
  Result := CompareInteger(SR1.First^, SR2.First^);
  if Result <> 0 then exit;
  Count := SR1.Count;
  // Setup First & Last pointers
  First1 := SR1.First;
  First2 := SR2.First;
  Last1 := First1; inc(Last1, Count);
  Last2 := First2; inc(Last2, Count);
  // Compare each time last ptrs then first ptrs, until they meet in the middle
  repeat
    dec(Last1); dec(Last2);
    if First1 = Last1 then exit;
    Result := CompareInteger(Last1^, Last2^);
    if Result <> 0 then exit;
    inc(First1); inc(First2);
    if First1 = Last1 then exit;
    Result := CompareInteger(First1^, First2^);
    if Result <> 0 then exit;
  until False;
end;

{ TsdRefString }

function TsdRefString.AsString: string;
begin
  Result := sdSRToString(StringRec);
end;

function TsdRefString.CompareToSR(const SR: TsdStringRec): integer;
begin
  if SR.Count = 0 then
  begin
    // shortcut
    Result := 1;
    exit;
  end;
  Result := sdCompareSR(StringRec, SR);
end;

destructor TsdRefString.Destroy;
begin
  FreeMem(FFirst);
  inherited;
end;

procedure TsdRefString.SetString(const SR: TsdStringRec);
begin
  FCount := SR.Count;
  ReallocMem(FFirst, FCount);
  Move(SR.First^, FFirst^, FCount);
end;

function TsdRefString.StringRec: TsdStringRec;
begin
  Result.First := FFirst;
  Result.Count := FCount;
end;

{ TsdRefStringList }

function TsdRefStringList.GetItems(Index: integer): TsdRefString;
begin
  Result := Get(Index);
end;

function TsdRefStringList.IndexOfID(AStringID: integer; var Index: integer): boolean;
var
  Min, Max: integer;
begin
  Result := False;
  // Find position - binary method
  Index := 0;
  Min := 0;
  Max := Count;
  while Min < Max do begin
    Index := (Min + Max) div 2;
    case CompareInteger(Items[Index].FStringID, AStringID) of
    -1: Min := Index + 1;
     0: begin
          Result := True;
          exit;
        end;
     1: Max := Index;
    end;
  end;
  Index := Min;
end;

function TsdRefStringList.IndexOfSR(const AStringRec: TsdStringRec; var Index: integer): boolean;
var
  Min, Max: integer;
  SR: TsdStringRec;
begin
  Result := False;
  // Find position - binary method
  Index := 0;
  Min := 0;
  Max := Count;
  while Min < Max do begin
    Index := (Min + Max) div 2;
    SR := TsdRefString(Get(Index)).StringRec;
    case sdCompareSR(SR, AStringRec) of
    -1: Min := Index + 1;
     0: begin
          Result := True;
          exit;
        end;
     1: Max := Index;
    end;
  end;
  Index := Min;
end;

{ TsdStringTable }

function TsdStringTable.AddString(const S: string): integer;
var
  SR: TsdStringRec;
begin
  SR := sdStringToSR(S);
  Result := AddStringRec(SR);
end;

function TsdStringTable.AddStringRec(const SR: TsdStringRec): integer;
var
  BySRIndex: integer;
  Item: TsdRefString;
  NewSR: TsdStringRec;
  Res: boolean;
begin
  // zero-length string
  if SR.Count = 0 then
  begin
    Result := 0;
    exit;
  end;

  // Try to find the new string
  if FBySR.IndexOfSR(SR, BySRIndex) then
  begin
    Item := FBySR.Items[BySRIndex];
    inc(Item.FRefCount);
    Result := Item.FStringID;
    exit;
  end;

  // Not found.. must make new item
  Item := TsdRefString.Create;
  Item.SetString(SR);
  NewSR := Item.StringRec;
  Item.FStringID := NextUniqueID;
  FById.Add(Item);
  Item.FRefCount := 1;

  // debug:
  //SetLength(Item.FValue, Item.FCount);
  //Move(Item.FirstPtr(FBase)^, Item.FValue[1], Item.FCount);
  // Insert in BySR lists
  Res := FBySR.IndexOfSR(NewSR, BySRIndex);
  assert(Res = False);
  FBySR.Insert(BySRIndex, Item);
  Result := Item.FStringID;
end;

procedure TsdStringTable.Clear;
begin
  FByID.Clear;
  FBySR.Clear;
end;

constructor TsdStringTable.Create;
begin
  inherited Create;
  FByID := TsdRefStringList.Create(False);
  FBySR := TsdRefStringList.Create(True);
end;

procedure TsdStringTable.DecRefCount(AItem: TsdRefString; ByIdIndex: integer);
var
  BySRIndex: integer;
  Res: boolean;
begin
  dec(AItem.FRefCount);
  assert(AItem.FRefCount >= 0);
  if AItem.FRefCount = 0 then
  begin
    // We must remove it
    FById.Delete(ByIdIndex);
    Res := FBySR.IndexOfSR(AItem.StringRec, BySRIndex);
    assert(Res = True);
    FBySR.Delete(BySRIndex);
  end;
end;

destructor TsdStringTable.Destroy;
begin
  FreeAndNil(FByID);
  FreeAndNil(FBySR);
  inherited;
end;

function TsdStringTable.GetString(ID: integer): string;
var
  Index, Count: integer;
  Item: TsdRefString;
begin
  if ID = 0 then begin
    Result := '';
    exit;
  end;
  // Find the ID
  if FByID.IndexOfID(ID, Index) then
  begin
    Item := FById[Index];
    Count := Item.FCount;
    SetLength(Result, Count);
    Move(Item.FFirst^, Result[1], Count);
    exit;
  end;
  Result := '';
end;

function TsdStringTable.NextUniqueID: integer;
begin
  if FById.Count = 0 then
    Result := 1
  else
    Result := FByID[FByID.Count - 1].FStringID + 1;
end;

procedure TsdStringTable.SaveToFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TsdStringTable.SaveToStream(S: TStream);
var
  i: integer;
  R: string;
begin
  for i := 0 to FBySR.Count - 1 do
  begin
    R := FBySR[i].AsString + #13#10;
    S.Write(R[1], length(R));
  end;
end;

procedure TsdStringTable.SetString(var ID: integer; const S: string);
var
  ByIdIndex: integer;
  Item: TsdRefString;
  SR: TsdStringRec;
begin
  // Make temp string record
  SR := sdStringtoSR(S);

  // Do we have a ref string with this ID?
  if (ID > 0) and FByID.IndexOfID(ID, ByIdIndex) then
  begin
    // Is the string still the same?
    Item := FById[ByIdIndex];
    if Item.CompareToSR(SR) = 0 then
      exit;
    // The string changed..
    DecRefCount(Item, ByIdIndex);
  end;
  ID := AddStringRec(SR);
end;

function TsdStringTable.StringCount: integer;
begin
  Result := FBySR.Count;
end;

end.
