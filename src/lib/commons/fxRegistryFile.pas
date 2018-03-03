unit fxRegistryFile;

interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: fxRegistryFile.pas
//       Author: Jaimy Azle (nospam@log.web.id)
//    Date Code: dd.mm.yyyy
// ========================================================================
//    Copyright: Seluruh isi dari file ini dilindungi oleh undang-undang
//               hak cipta dan kesepakatan internasional. Segala bentuk
//               reproduksi, reverse-engineering, dan distribusi atas
//               seluruh atau bagian apapun dari kode yang ada dalam file
//               ini tanpa ijin tertulis merupakan pelanggaran hukum dan
//               akan dituntut ke pengadilan dengan sanksi maksimum yang
//               ada.
//
// Code Version: (3rd Generation Code)
// ========================================================================
//  Description:
//    TfxRegFile is a TRegistry like that store informations in binary file,
//    not the windows system registry. It has the same basic functions as in
//    TRegistry.
//
//    This version is a major rewrite version, thus will not compatible with
//    datafile created by TfxRegFile v1.0.
//
//    WHAT'S NEW:
//      * indexed entry, making searching entry will be faster than before
//      * ability to reuse space
//      * ability to compact datafile, removing any unused space
//      * up to 1,000,000 record (i hope this is enough since we were not
//        talking about full featured database engine, it is a registry file)
//
//    This unit use some routines from FastCode Project
//    http://dennishomepage.gugs-cats.dk/FastCodeProject.htm
//
//      * FillCharJOH_FPU
//      * MoveJOH_IA32
//
//    thanks to FastCode people for this great code.
//
// ========================================================================
// UPDATE:
//
// 01-01-2006: Bug on DeleteKey fixed!
//
///////////////////////////////////////////////////////////////////////////////
uses
  SysUtils, Classes;

const
  fxIdent             = 'REG$';
  fxFileVer           = '2.00';
  fxNameLen           = 48;
  fxRootID            = 'RDB$ROOT';
  fxIndexBlock        = 1024;
  fxCacheSize         = 64;


type
  TfxIntegerArray     = array of Integer;
  TfxMaxIntArray      = array [0..MaxInt DIV SizeOf(Integer)-1] of Integer;

  PInteger            = ^Integer;
  PfxMaxIntArray      = ^TfxMaxIntArray;

  EfxRegFileError  = Exception;

  PfxRegFileHeader = ^TfxRegFileHeader;
  TfxRegFileHeader = packed record
    Ident       : array[0..3] of Char;
    FileVer     : array[0..3] of Char;
    LastID      : Integer;
    Encrypted   : Boolean;
    RecordCount : Integer;
    IndexCount  : integer;
    IndexOffset : integer;
    DataOffset  : integer;
    FDOffset    : integer;
    LDOffset    : integer;
  end;

  TfxIdentName   =  array[0..fxNameLen-1] of Char;
  TfxRegType     = (rtKey = 0, rtValue);
  TfxRegDataType = (rdUnknown = 0, rdBinary, rdString, rdExpandString,
                    rdKey, rdInteger, rdCardinal, rdShortInt, rdSmallInt,
                    rdLongint, rdInt64, rdByte, rdWord, rdLongword,
                    rdReal48, rdSingle, rdDouble, rdExtended, rdComp,
                    rdCurrency, rdReal, rdBoolean, rdDateTime);

  TfxRegDataInfo = record
    RegData: TfxRegDataType;
    DataSize: Integer;
  end;

  TfxFindRec = packed record
    Found       : integer;
    LastPos     : integer;
    PID         : integer;
    RegType     : TfxRegType;
  end;

  PfxRegDataHeader = ^TfxRegDataHeader;
  TfxRegDataHeader = packed record
    Ident       : TfxIdentName;
    ID          : Integer;
    PID         : Integer;
    RegType     : TfxRegType;
    DataType    : TfxRegDataType;
    DataOffset  : Integer;
    DataSize    : Integer;
    EmptyData   : Boolean;
    IntData     : array[0..9] of byte;
    Deleted     : Boolean;
    NDOffset    : integer;
  end;

  TfxListSortCompare = function (Item1, Item2: Pointer): Integer of object;
  TfxRecList = class(TList)
  public
    procedure Reindex(Compare: TfxListSortCompare);
  end;

  pfxRegCacheRec = ^TfxRegCacheRec;
  TfxRegCacheRec = record
    DataOffset  : integer;
    RegHeader   : TfxRegDataHeader;
  end;

  TfxRegCacheManager = class(TObject)
  private
    FCachedReg: array of PfxRegCacheRec;
    FCount: integer;
    FSize: integer;
    function GetItem(Index: Integer): TfxRegDataHeader;
  public
    constructor Create(const ASize: integer);
    destructor Destroy; override;

    function FindOffset(AOffset: Integer): integer;

    procedure Clear;
    procedure Push(const AOffset: integer; AHdr: TfxRegDataHeader);
    procedure Replace(const AIndex, NewOffset: integer;
                      AHdr: TfxRegDataHeader);
    procedure Delete(const AIndex: integer);

    property Items[Index: Integer]: TfxRegDataHeader read GetItem;
    property Count: integer read FCount;
  end;

  TfxRegFile = class(TObject)
  private
    FInternalStream: TFileStream;
    FFileStream: TStream;
    FRecOffsetList: TfxRecList;
    FFileHeader: TfxRegFileHeader;
    FPDataHeader: TfxRegDataHeader;
    FPDataOffset: Integer;
    FKeyTree: string;
    FRegCacheManager: TfxRegCacheManager;

    function InitIndexBlock: integer;

    procedure Initialize;
    procedure ReadFileHeader;
    procedure WriteFileHeader;
    procedure ReadRecordData(const AOffset: Integer; Buffer: TStream);
    procedure WriteRecordData(Buffer: TStream);
    procedure ParseKeys(AKeys: string; AStrings: TStringList);
    function GetIndexOffset(AItemIndex: Integer): integer;
    function GetParentKeyName: string;
    function GetCurrentParentID: integer;
    function GetItemPos(Index: Integer): Integer;
    function GetIndexCount: integer;
  protected
    function InternalFind(const AKeyName: string; const AKeyType: TfxRegType;
                          var pHdr: TfxRegDataHeader;
                          SCompare: TfxListSortCompare): TfxFindRec;
    function InternalFindFirst(const APID: integer; const AKeyType: TfxRegType;
                          var pHdr: TfxRegDataHeader): TfxFindRec;
    function InternalFindNext(var FindRec: TfxFindRec;
                          var pHdr: TfxRegDataHeader): boolean;

    function InternalCreateDataHeader(pHdr: PfxRegDataHeader;
                          pBuf: pointer): Integer;

    procedure InternalLoadIndex;
    procedure InternalCreateKey(const KeyName: string);
    procedure InternalDelete(Offset: integer; pHdr: PfxRegDataHeader);
    procedure InternalUpdateIndex(AFromIndex: integer);
    procedure InternalReadRootKey;
    function InternalOpenKey(const AKeyName: string;
      const CanCreate: boolean = True): boolean;

    function DefFullCompare(Item1, Item2: Pointer): Integer;    
    function DefaultIndexSort(Item1, Item2: Pointer): Integer;
    function DefaultFindCompare(Item1, Item2: Pointer): Integer;
    function DefaultListFindCompare(Item1, Item2: Pointer): Integer;
    function DefaultCompareID(Item1, Item2: Pointer): Integer;

    function FindInsertPos(const DataType, ReqSize: integer): integer;
  public
    constructor Create(AStream: TStream); overload;
    constructor Create(const AFileName: string); overload;
    destructor Destroy; override;

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    function GetRecordHeader(AOffset: integer): TfxRegDataHeader;

    function OpenKey(const AKeyNames: string;
      const CanCreate: boolean = True): boolean;
    procedure CreateKey(const Key: string);

    function HasSubKeys: Boolean;
    function KeyExists(const Key: string): Boolean;
    function ValueExists(const Name: string): Boolean;

    procedure GetKeyNames(Strings: TStrings);
    procedure GetValueNames(Strings: TStrings);
    function GetDataInfo(const ValueName: string;
                         var Value: TfxRegDataInfo): Boolean;
    function GetDataSize(const ValueName: string): Integer;
    function GetDataType(const ValueName: string): TfxRegDataType;

    procedure MoveUpKey;
    procedure CloseKey;

    procedure DeleteKey(const Key: string);
    procedure DeleteValue(const Name: string);

    procedure Reindex;

    procedure ReadStream(const Name: string; AStream: TStream);
    function ReadBool(const Name: string): Boolean;
    function ReadDate(const Name: string): TDateTime;
    function ReadDateTime(const Name: string): TDateTime;
    function ReadFloat(const Name: string): Double;
    function ReadInteger(const Name: string): Integer;
    function ReadString(const Name: string): string;
    function ReadTime(const Name: string): TDateTime;
    function ReadCurrency(const Name: string): Currency;
    procedure ReadStrings(const Name: string; List: TStrings);

    procedure Compact; overload;
    procedure Compact(const TmpFileName: string); overload;

    procedure Clear;

    procedure WriteCurrency(const Name: string; Value: Currency);
    procedure WriteBinaryData(const Name: string;
                              const DataType: TfxRegDataType;
                              Buffer: pointer;
                              const BufSize: Integer);
    procedure WriteStream(const Name: string; Stream: TStream);
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDate(const Name: string; Value: TDateTime);
    procedure WriteDateTime(const Name: string; Value: TDateTime);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteString(const Name, Value: string);
    procedure WriteTime(const Name: string; Value: TDateTime);
    procedure WriteStrings(const Name: string; List: TStrings); 

    property Stream: TStream read FFileStream;
    property CurrentPath: string read FKeyTree;
    property CurrentKeyName: string read GetParentKeyName;
    property CurrentParentID: integer read GetCurrentParentID;

    property ItemPos[Index: Integer]: Integer read GetItemPos;
    property IndexCount: integer read GetIndexCount;
  end;


implementation
uses
  Math;

resourcestring
  SInvalidFileFormat    = 'Registry file format is invalid or file is corrupt';
  SFileWriteError       = 'File write error';
  SFileReadError        = 'File read error';
  SIndexFileCorruptError= 'Index file corrupt error';
  SRegfileCorruptError  = 'Corrupted registry file found';
  SDataTypeError        = 'Value is not assigned with current data type';


//Author:            John O'Harrow
//Date:              N/A
//Optimized for:     RTL
//Instructionset(s): IA32
//Original Name:     FillCharJOH_FPU
procedure FastFillChar(var Dest; count: Integer; Value: Byte);
asm {Size = 153 Bytes}
  cmp   edx, 32
  mov   ch, cl                    {Copy Value into both Bytes of CX}
  jl    @@Small
  mov   [eax  ], cx               {Fill First 8 Bytes}
  mov   [eax+2], cx
  mov   [eax+4], cx
  mov   [eax+6], cx
  sub   edx, 16
  fld   qword ptr [eax]
  fst   qword ptr [eax+edx]       {Fill Last 16 Bytes}
  fst   qword ptr [eax+edx+8]
  mov   ecx, eax
  and   ecx, 7                    {8-Byte Align Writes}
  sub   ecx, 8
  sub   eax, ecx
  add   edx, ecx
  add   eax, edx
  neg   edx
@@Loop:
  fst   qword ptr [eax+edx]       {Fill 16 Bytes per Loop}
  fst   qword ptr [eax+edx+8]
  add   edx, 16
  jl    @@Loop
  ffree st(0)
  ret
  nop
  nop
  nop
@@Small:
  test  edx, edx
  jle   @@Done
  mov   [eax+edx-1], cl       {Fill Last Byte}
  and   edx, -2               {No. of Words to Fill}
  neg   edx
  lea   edx, [@@SmallFill + 60 + edx * 2]
  jmp   edx
  nop                             {Align Jump Destinations}
  nop
@@SmallFill:
  mov   [eax+28], cx
  mov   [eax+26], cx
  mov   [eax+24], cx
  mov   [eax+22], cx
  mov   [eax+20], cx
  mov   [eax+18], cx
  mov   [eax+16], cx
  mov   [eax+14], cx
  mov   [eax+12], cx
  mov   [eax+10], cx
  mov   [eax+ 8], cx
  mov   [eax+ 6], cx
  mov   [eax+ 4], cx
  mov   [eax+ 2], cx
  mov   [eax   ], cx
  ret {DO NOT REMOVE - This is for Alignment}
@@Done:
end;

//Author:            John O'Harrow
//Date:              9/8-03
//Optimized for:     RTL Replacement (without MMX)
//Instructionset(s): IA32
//Original name:     MoveJOH_IA32
procedure FastMove(const Source; var Dest; Count : Integer);
const
  TABLESIZE = 24;
asm
  cmp   eax,edx
  jle   @Check
@FowardMove:
  cmp   ecx,TABLESIZE
  jg    @FwdNotSmall
  add   eax,ecx
@ForwardMove2:
  add   edx,ecx {End Dest}
  or    ecx,ecx {For Compatibility with Delphi's move for Count <= 0}
  jg    @FwdSmall
@Done:
  ret
@Check:
  je    @Done {For Compatibility with Delphi's move for Source = Dest}
@CheckOverlap:
  add   eax,ecx {End Source}
  cmp   eax,edx
  jg    @Backwards {Source/Dest Overlap}
  cmp   ecx,TABLESIZE {No Overlap, Use Forward Move}
  jle   @ForwardMove2 {Source already incremented by Count}
  sub   eax,ecx {Restore Original Source}
@FwdNotSmall:
  push  ebx
  mov   ebx,edx
  fild  qword ptr [eax]
  add   eax,ecx {QWORD Align Writes}
  add   ecx,edx
  add   edx,7
  and   edx,-8
  sub   ecx,edx
  add   edx,ecx {Now QWORD Aligned}
  sub   ecx,16
  neg   ecx
@FwdLoop:
  fild  qword ptr [eax+ecx-16]
  fistp qword ptr [edx+ecx-16]
  fild  qword ptr [eax+ecx-8]
  fistp qword ptr [edx+ecx-8]
  add   ecx,16
  jle   @FwdLoop
  fistp qword ptr [ebx]
  neg   ecx
  add   ecx,16
  pop   ebx
@FwdSmall:
  jmp   dword ptr [@FwdJumpTable+ecx*4]
@Backwards: {Overlapping Source/Dest}
  sub   eax,ecx {Restore Original Source}
  cmp   ecx,TABLESIZE
  jle   @BwdSmall
@BwdNotSmall:
  push  ebx
@BackwardMove:
  fild  qword ptr [eax+ecx-8]
  lea   ebx,[edx+ecx] {QWORD Align Writes}
  and   ebx,7
  sub   ecx,ebx
  add   ebx,ecx {Now QWORD Aligned, EBX = Original Length}
  sub   ecx,16
@BwdLoop:
  fild  qword ptr [eax+ecx]
  fild  qword ptr [eax+ecx+8]
  fistp qword ptr [edx+ecx+8]
  fistp qword ptr [edx+ecx]
  sub   ecx,16
  jge   @BwdLoop
  fistp qword ptr [edx+ebx-8]
  add   ecx,16
  pop   ebx
@BwdSmall:
  jmp   dword ptr [@BwdJumpTable+ecx*4]
@FwdJumpTable:
  dd    @Done
  dd    @Fwd01,@Fwd02,@Fwd03,@Fwd04,@Fwd05,@Fwd06,@Fwd07,@Fwd08
  dd    @Fwd09,@Fwd10,@Fwd11,@Fwd12,@Fwd13,@Fwd14,@Fwd15,@Fwd16
  dd    @Fwd17,@Fwd18,@Fwd19,@Fwd20,@Fwd21,@Fwd22,@Fwd23,@Fwd24
@BwdJumpTable:
  dd    @Done
  dd    @Bwd01,@Bwd02,@Bwd03,@Bwd04,@Bwd05,@Bwd06,@Bwd07,@Bwd08
  dd    @Bwd09,@Bwd10,@Bwd11,@Bwd12,@Bwd13,@Bwd14,@Bwd15,@Bwd16
  dd    @Bwd17,@Bwd18,@Bwd19,@Bwd20,@Bwd21,@Bwd22,@Bwd23,@Bwd24
@Fwd24:
  mov   ecx,[eax-24]
  mov   [edx-24],ecx
@Fwd20:
  mov   ecx,[eax-20]
  mov   [edx-20],ecx
@Fwd16:
  mov   ecx,[eax-16]
  mov   [edx-16],ecx
@Fwd12:
  mov   ecx,[eax-12]
  mov   [edx-12],ecx
@Fwd08:
  mov   ecx,[eax-8]
  mov   [edx-8],ecx
@Fwd04:
  mov   ecx,[eax-4]
  mov   [edx-4],ecx
  ret
@Fwd23:
  mov   ecx,[eax-23]
  mov   [edx-23],ecx
@Fwd19:
  mov   ecx,[eax-19]
  mov   [edx-19],ecx
@Fwd15:
  mov   ecx,[eax-15]
  mov   [edx-15],ecx
@Fwd11:
  mov   ecx,[eax-11]
  mov   [edx-11],ecx
@Fwd07:
  mov   ecx,[eax-7]
  mov   [edx-7],ecx
@Fwd03:
  mov   cx,[eax-3]
  mov   [edx-3],cx
  mov   cl,[eax-1]
  mov   [edx-1],cl
  ret
@Fwd22:
  mov   ecx,[eax-22]
  mov   [edx-22],ecx
@Fwd18:
  mov   ecx,[eax-18]
  mov   [edx-18],ecx
@Fwd14:
  mov   ecx,[eax-14]
  mov   [edx-14],ecx
@Fwd10:
  mov   ecx,[eax-10]
  mov   [edx-10],ecx
@Fwd06:
  mov   ecx,[eax-6]
  mov   [edx-6],ecx
@Fwd02:
  mov   cx,[eax-2]
  mov   [edx-2],cx
  ret
@Fwd21:
  mov   ecx,[eax-21]
  mov   [edx-21],ecx
@Fwd17:
  mov   ecx,[eax-17]
  mov   [edx-17],ecx
@Fwd13:
  mov   ecx,[eax-13]
  mov   [edx-13],ecx
@Fwd09:
  mov   ecx,[eax-9]
  mov   [edx-9],ecx
@Fwd05:
  mov   ecx,[eax-5]
  mov   [edx-5],ecx
@Fwd01:
  mov   cl,[eax-1]
  mov   [edx-1],cl
  ret
@Bwd24:
  mov   ecx,[eax+20]
  mov   [edx+20],ecx
@Bwd20:
  mov   ecx,[eax+16]
  mov   [edx+16],ecx
@Bwd16:
  mov   ecx,[eax+12]
  mov   [edx+12],ecx
@Bwd12:
  mov   ecx,[eax+8]
  mov   [edx+8],ecx
@Bwd08:
  mov   ecx,[eax+4]
  mov   [edx+4],ecx
@Bwd04:

  mov   ecx,[eax]
  mov   [edx],ecx
  ret
@Bwd23:
  mov   ecx,[eax+19]
  mov   [edx+19],ecx
@Bwd19:
  mov   ecx,[eax+15]
  mov   [edx+15],ecx
@Bwd15:
  mov   ecx,[eax+11]
  mov   [edx+11],ecx
@Bwd11:
  mov   ecx,[eax+7]
  mov   [edx+7],ecx
@Bwd07:
  mov   ecx,[eax+3]
  mov   [edx+3],ecx
@Bwd03:
  mov   cx,[eax+1]
  mov   [edx+1],cx
  mov   cl,[eax]
  mov   [edx],cl
  ret
@Bwd22:
  mov   ecx,[eax+18]
  mov   [edx+18],ecx
@Bwd18:
  mov   ecx,[eax+14]
  mov   [edx+14],ecx
@Bwd14:
  mov   ecx,[eax+10]
  mov   [edx+10],ecx
@Bwd10:
  mov   ecx,[eax+6]
  mov   [edx+6],ecx
@Bwd06:
  mov   ecx,[eax+2]
  mov   [edx+2],ecx
@Bwd02:
  mov   cx,[eax]
  mov   [edx],cx
  ret
@Bwd21:
  mov   ecx,[eax+17]
  mov   [edx+17],ecx
@Bwd17:
  mov   ecx,[eax+13]
  mov   [edx+13],ecx
@Bwd13:
  mov   ecx,[eax+9]
  mov   [edx+9],ecx
@Bwd09:
  mov   ecx,[eax+5]
  mov   [edx+5],ecx
@Bwd05:
  mov   ecx,[eax+1]
  mov   [edx+1],ecx
@Bwd01:
  mov   cl,[eax]
  mov   [edx],cl
end;

{ TfxRecList }
procedure QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TfxListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L; J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do Inc(I);
      while SCompare(SortList^[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I); Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TfxRecList.Reindex(Compare: TfxListSortCompare);
begin
  if (List <> nil) and (Count > 0) then
    QuickSort(List, 0, Count - 1, Compare);
end;

{ TfxRegCacheManager }

procedure TfxRegCacheManager.Clear;
var
  pHdr: pfxRegCacheRec;
begin
  if Count > 0 then
  while FCount > 0 do
  begin
    pHdr := FCachedReg[FCount-1];
    if (pHdr <> nil) then
      FreeMem(pHdr, SizeOf(TfxRegCacheRec));
    FCachedReg[FCount-1] := nil;
    Dec(FCount);
  end;
end;

constructor TfxRegCacheManager.Create(const ASize: integer);
begin
  SetLength(FCachedReg, ASize);
  FSize := ASize;
end;

procedure TfxRegCacheManager.Delete(const AIndex: integer);
var
  PCachedRec: PfxRegCacheRec;
  i: integer;
begin
  if ((AIndex >= 0) and (AIndex < FCount)) then
  begin
    PCachedRec := FCachedReg[AIndex];
    if (PCachedRec <> nil) then
      FreeMem(PCachedRec, SizeOf(TfxRegCacheRec));
    if FCount > 0 then
      for i := AIndex to (FCount-2)  do
        FCachedReg[i] := FCachedReg[i+1];
    FCachedReg[FCount-1] := nil;
    Dec(FCount);
  end;
end;

destructor TfxRegCacheManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TfxRegCacheManager.FindOffset(AOffset: Integer): integer;
var
  Found: boolean;
  i: integer;
  Rec: pfxRegCacheRec;
begin
  Found := False;
  I := -1;
  if FCount > 0 then
  for i := 0 to FCount-1 do
  begin
    if FCachedReg[i]^.DataOffset = AOffset then
    begin
      Found := True;
      break;
    end;
  end;
  {
    Pindahkan index yang ditemukan ke top list dengan demikian
    yang cache record yang jarang digunakan akan berada pada
    posisi bottom dalam list dan otomatis dihapus saat ada
    record baru dimasukkan.
  }
  if Found then
  begin
    Rec := FCachedReg[i];
    FCachedReg[i] := FCachedReg[0];
    FCachedReg[0] := Rec;
    Result := 0;
  end else
    Result := -1;
end;

function TfxRegCacheManager.GetItem(Index: Integer): TfxRegDataHeader;
begin
  FastFillChar(Result, SizeOf(TfxRegDataHeader), 0);
  if (FCount > 0) and ((Index >= 0) and (index < FCount)) then
    FastMove(FCachedReg[Index]^.RegHeader, Result, SizeOf(TfxRegDataHeader));
end;

procedure TfxRegCacheManager.Push(const AOffset: integer;
  AHdr: TfxRegDataHeader);
var
  PCachedRec: PfxRegCacheRec;
  i: integer;
begin
  if FindOffset(AOffset) < 0 then
  begin
    if (FCount >= FSize) then
    begin
      PCachedRec := FCachedReg[FCount-1];
      if (PCachedRec <> nil) then
        FreeMem(PCachedRec, SizeOf(TfxRegCacheRec));
      Dec(FCount);
    end;
    if FCount > 0 then
      for i := FCount-1 downto 0 do
        FCachedReg[i+1] := FCachedReg[i];
    GetMem(PCachedRec, SizeOf(TfxRegCacheRec));
    PCachedRec^.DataOffset := AOffset;
    FastMove(AHdr, PCachedRec^.RegHeader, SizeOf(TfxRegDataHeader));
    FCachedReg[0] := PCachedRec;
    Inc(FCount);
  end;
end;

procedure TfxRegCacheManager.Replace(const AIndex, NewOffset: integer;
  AHdr: TfxRegDataHeader);
var
  PCachedRec: PfxRegCacheRec;
begin
  PCachedRec := FCachedReg[AIndex];
  PCachedRec^.DataOffset := NewOffset;
  FastMove(AHdr, PCachedRec^.RegHeader, SizeOf(TfxRegDataHeader));
end;

{ TfxRegFile }
procedure Touch(const AFileName: string);
var
  AHandle: integer;
begin
  AHandle := FileCreate(AFileName);
  if AHandle <> -1 then
    FileClose(AHandle);
end;

constructor TfxRegFile.Create(AStream: TStream);
begin
  FRecOffsetList := TfxRecList.Create;
  FRegCacheManager := TfxRegCacheManager.Create(fxCacheSize);
  FInternalStream := nil;
  FFileStream := AStream;
  Initialize;
end;

constructor TfxRegFile.Create(const AFileName: string);
var
  OpenMode : integer;
begin
  FRecOffsetList := TfxRecList.Create;
  FRegCacheManager := TfxRegCacheManager.Create(fxCacheSize);
  OpenMode := fmOpenReadWrite or fmShareDenyNone;
  if not FileExists(AFileName)then
    Touch(AFileName);
  FInternalStream := TFileStream.Create(AFileName, OpenMode);
  FFileStream := FInternalStream;
  Initialize;
end;

destructor TfxRegFile.Destroy;
begin
  FRegCacheManager.Free;
  FRecOffsetList.Free;
  if (FInternalStream <> nil) then
    FInternalStream.Free;
  inherited Destroy;
end;

function TfxRegFile.GetRecordHeader(AOffset: integer): TfxRegDataHeader;
var
  APos, AIndex: integer;
begin
  APos := Stream.Position;
  try
    FastFillChar(Result, SizeOf(TfxRegDataHeader), 0);
    AIndex := FRegCacheManager.FindOffset(AOffset);
    if AIndex <> -1 then
      Result := FRegCacheManager.Items[AIndex]
    else begin
      Stream.Seek(AOffset, soFromBeginning);
      if Stream.Read(Result, SizeOf(TfxRegDataHeader)) <>
         SizeOf(TfxRegDataHeader) then
        raise EfxRegFileError.Create(SFileReadError);
      FRegCacheManager.Push(AOffset, Result);
    end;
  finally
    Stream.Seek(APos, soFromBeginning);
  end;
end;

function TfxRegFile.GetIndexOffset(AItemIndex: Integer): integer;
var
  ItemIndex: integer;
  IndexOffset: integer;
  APos: integer;
begin
  IndexOffset := 0;
  APos := Stream.Position;
  try
    ItemIndex := (AItemIndex div fxIndexBlock);
    Stream.Position := FFileHeader.IndexOffset + (SizeOf(Integer) * ItemIndex);
    if Stream.Read(IndexOffset, SizeOf(Integer)) <> SizeOf(Integer) then
      raise EfxRegFileError.Create(SFileReadError);
    if IndexOffset = 0 then
    begin
      IndexOffset := InitIndexBlock;
      Stream.Position := FFileHeader.IndexOffset +
                          (SizeOf(Integer) * ItemIndex);
      Stream.Write(IndexOffset, SizeOf(Integer));
    end;
  finally
    Stream.Position := APos;
    Result := IndexOffset;
  end;
end;


procedure TfxRegFile.Initialize;
  procedure InitFileHeader;
  begin
    FFileHeader.Ident    := fxIdent;
    FFileHeader.FileVer  := fxFileVer;
    FFileHeader.Encrypted:= False;
  end;

begin
  FRecOffsetList.Clear;
  FRegCacheManager.Clear;
  FFileStream.Position := 0;
  FastFillChar(FFileHeader, SizeOf(TfxRegFileHeader), 0);
  FastFillChar(FPDataHeader, SizeOf(TfxRegDataHeader), 0);
  FPDataOffset := 0;
  if (FFileStream.Size < SizeOf(TfxRegFileHeader)) then
  begin
    InitFileHeader;
    WriteFileHeader;
    { Init list of index block offset }
    FFileHeader.IndexOffset :=  InitIndexBlock;
    { Update file header }
    WriteFileHeader;
  end else
  begin
    ReadFileHeader;
    InternalLoadIndex;
  end;
  { if Record is null then create root key }
  if FFileHeader.RecordCount = 0 then
  begin
    InternalCreateKey(fxRootID);
    FFileHeader.DataOffset := FPDataOffset;
    WriteFileHeader;
  end;
end;

function TfxRegFile.InitIndexBlock: integer;
var
  pBuf: Pointer;
  APos: integer;
begin
  { Prepare Index Block }
  GetMem(pBuf, SizeOf(integer) * fxIndexBlock);
  try
    FastFillChar(pBuf^, SizeOf(integer) * fxIndexBlock, 0);
    Stream.Seek(0, soFromEnd);
    { Index Tab list }
    Result := Stream.Position;
    if (Stream.Write(pBuf^, SizeOf(Integer) * fxIndexBlock) <>
       (SizeOf(Integer) * fxIndexBlock)) then
      raise EfxRegFileError.Create(SFileWriteError);
    { First Index Block }
    APos  := Stream.Position;
    if (Stream.Write(pBuf^, SizeOf(Integer) * fxIndexBlock) <>
       (SizeOf(Integer) * fxIndexBlock)) then
      raise EfxRegFileError.Create(SFileWriteError);
    { Update Index Tab }
    Stream.Seek(Result, soFromBeginning);
    if (Stream.Write(APos, SizeOf(Integer)) <>
        SizeOf(Integer)) then
      raise EfxRegFileError.Create(SFileWriteError);
  finally
    FreeMem(pBuf, SizeOf(integer) * fxIndexBlock);
  end;
end;

function TfxRegFile.InternalCreateDataHeader(
  pHdr: PfxRegDataHeader; pBuf: pointer): Integer;
var
  FindRec: TfxFindRec;
  AHdr: TfxRegDataHeader;
  BPos: integer;
  IndexOffset, IndexPos, NewItemIndex : integer;

  procedure FindInsertingPoint;
  var
    CompRes: integer;
  begin
    if (FindRec.LastPos >= 0) and
       (FindRec.LastPos < FRecOffsetList.Count) then
    begin
      FindRec.Found := DefFullCompare(pHdr, @AHdr);
      Compres := FindRec.Found;
      if FindRec.Found < 0 then
      begin
        while true do
        begin
          Dec(FindRec.LastPos);
          if FindRec.LastPos <= 0 then
          begin
            Inc(FindRec.LastPos);
            Break;
          end;
          AHdr := GetRecordHeader(
                    Integer(
                      FRecOffsetList.Items[FindRec.LastPos]
                    )
                  );
          CompRes := DefFullCompare(pHdr, @AHdr);
          if not (CompRes <= 0) then
          begin
            Inc(FindRec.LastPos);
            break;
          end;
        end;
      end else
      begin
        while true do
        begin
          Inc(FindRec.LastPos);
          if FindRec.LastPos >= FRecOffsetList.Count then
            Break;
          AHdr := GetRecordHeader(
                    Integer(
                      FRecOffsetList.Items[FindRec.LastPos]
                    )
                  );
          CompRes := DefFullCompare(pHdr, @AHdr);
          if not (CompRes >= 0) then
          begin
            Dec(FindRec.LastPos);
            break;
          end;
        end;
      end;
      FindRec.Found := CompRes;
    end;
  end;

begin
  BPos := -1;
  try
    { record tersebut dah pernah ada blom? }
    FindRec := InternalFind(pHdr^.Ident, pHdr^.RegType, AHdr,
                  DefaultFindCompare);
    if (FindRec.Found = 0) then  // wis !!!
    begin
      pHdr^.ID := AHdr.ID;
      pHdr^.PID:= AHdr.PID;
      { cek tipe dan jenis data bukan variable size (stream/string) }
      if (Integer(pHdr^.DataType) > 3) and
         (Integer(AHdr.DataType) > 3) then
      begin
        Stream.Position := Integer(FRecOffsetList.Items[FindRec.LastPos]);
        BPos := Stream.Position;
        Stream.Write(pHdr^, SizeOf(TfxRegDataHeader));
      end else
      { perlakuan khusus untuk data string /stream }
      begin
        InternalDelete(Integer(FRecOffsetList.Items[FindRec.LastPos]), @AHdr);
        BPos := FindInsertPos(Integer(pHdr^.DataType), pHdr^.DataSize);
        Stream.Seek(BPos, soFromBeginning);
        if (pBuf <> nil) and
           (Integer(pHdr^.DataType) <= 3) then
              pHdr^.DataOffset := BPos + SizeOf(TfxRegDataHeader);
        Stream.Write(pHdr^, SizeOf(TfxRegDataHeader));
        if (pBuf <> nil) and
           (Integer(pHdr^.DataType) <= 3) and
           (Stream.Write(pBuf^, pHdr^.DataSize) <> pHdr^.DataSize) then
          raise EfxRegFileError.Create(SFileWriteError);
        { jika posisi fisik record pindah dari yang lama,
          update juga index-nya }
        if (Integer(FRecOffsetList.Items[FindRec.LastPos]) <> BPos) then
        begin
          FRecOffsetList.Items[FindRec.LastPos] := Pointer(BPos);
          { We need to update our index }
          IndexOffset := GetIndexOffset(FindRec.LastPos);
          IndexPos := (FindRec.LastPos mod fxIndexBlock) * SizeOf(Integer);
          Stream.Seek(IndexOffset+IndexPos, soFromBeginning);
          Stream.Write(BPos, SizeOf(Integer));
        end;
      end;
      { jika record ada juga di cache, pastikan cache tsb diupdate juga }
      IndexOffset := FRegCacheManager.FindOffset(
                      Integer(
                        FRecOffsetList.Items[FindRec.LastPos]
                      )
                     );
      if IndexOffset <> -1 then
        FRegCacheManager.Replace(IndexOffset, BPos, pHdr^);
    end else
    { gak, kita butuh entry record baru }
    begin
      { cari posisi yang tepat, reuse deleted space kalau bisa }
      BPos := FindInsertPos(Integer(pHdr^.DataType), pHdr^.DataSize);
      Stream.Seek(BPos, soFromBeginning);
      if (pHdr^.ID = 0) then
      begin
        Inc(FFileHeader.LastID);
        pHdr^.ID := FFileHeader.LastID;
      end else
        if FFileHeader.LastID < pHdr^.ID then
          FFileHeader.LastID := pHdr^.ID;
      if (pBuf <> nil) and
         (Integer(pHdr^.DataType) <= 3) then
        pHdr^.DataOffset := BPos + SizeOf(TfxRegDataHeader);
      Stream.Write(pHdr^, SizeOf(TfxRegDataHeader));
      if (pBuf <> nil) and
         (Integer(pHdr^.DataType) <= 3) and
         (Stream.Write(pBuf^, pHdr^.DataSize) <> pHdr^.DataSize) then
            raise EfxRegFileError.Create(SFileWriteError);
      Inc(FFileHeader.RecordCount);
      { Ok, update indexnya. Cari dulu tapi posisi yang tepat untuk
        record tersebut dalam index}
      FindInsertingPoint;
      if FindRec.Found > 0 then
      begin
        { tambahkan di akhir list }
        if (FindRec.LastPos >= FRecOffsetList.Count) or
           (FRecOffsetList.Count <= 0) then
        begin
          NewItemIndex := FRecOffsetList.Add(Pointer(BPos));
          Inc(FFileHeader.IndexCount);
          IndexOffset := GetIndexOffset(NewItemIndex);
          IndexPos := (NewItemIndex mod fxIndexBlock) * SizeOf(Integer);
          Stream.Seek(IndexOffset+IndexPos, soFromBeginning);
          Stream.Write(BPos, SizeOf(Integer));
        end else
        { sisipkan di tengah list index tersebut }
        begin
          FRecOffsetList.Insert(FindRec.LastPos, Pointer(BPos));
          Inc(FFileHeader.IndexCount);
          InternalUpdateIndex(FindRec.LastPos);
        end;
      end else
      begin
        if FRecOffsetList.Count > 0 then
        { sisipkan di tengah list index tersebut }
        begin
          FRecOffsetList.Insert(FindRec.LastPos, Pointer(BPos));
          FFileHeader.IndexCount := FRecOffsetList.Count;
          InternalUpdateIndex(FindRec.LastPos);
        end else
        { tambahkan di akhir list }
        begin
          NewItemIndex := FRecOffsetList.Add(Pointer(BPos));
          Inc(FFileHeader.IndexCount);
          IndexOffset := GetIndexOffset(NewItemIndex);
          IndexPos := (NewItemIndex mod fxIndexBlock) * SizeOf(Integer);
          Stream.Seek(IndexOffset+IndexPos, soFromBeginning);
          Stream.Write(BPos, SizeOf(Integer));
        end;
      end;
    end;
  finally
    Result := BPos;
    WriteFileHeader;
  end;
end;

procedure TfxRegFile.InternalCreateKey(const KeyName: string);
var
  RecHeader: TfxRegDataHeader;
begin
  FastFillChar(RecHeader, SizeOf(TfxRegDataHeader), 0);
  RecHeader.PID:= FPDataHeader.ID;
  StrPCopy(RecHeader.Ident, KeyName);
  RecHeader.RegType := rtKey;
  RecHeader.DataType:= rdKey;
  RecHeader.EmptyData := True;
  FPDataOffset := InternalCreateDataHeader(@RecHeader, nil);
  { empty default key and set the new key as current default key }
  FastFillChar(FPDataHeader, SizeOf(TfxRegDataHeader), 0);
  FastMove(RecHeader, FPDataHeader, SizeOf(TfxRegDataHeader));
end;

procedure TfxRegFile.InternalDelete(Offset: integer;
  pHdr: PfxRegDataHeader);
var
  APos     : Integer;
  Count    : Integer;
  AHdr     : TfxRegDataHeader;
begin
  pHdr^.Deleted := True;
  APos := Stream.Position;
  try
    Stream.Position := Offset;
    Count := Stream.Write(pHdr^, SizeOf(TfxRegDataHeader));
    if Count <> SizeOf(TfxRegDataHeader) then
      raise EfxRegFileError.Create(SFileWriteError);
    if FFileHeader.FDOffset = 0 then
      FFileHeader.FDOffset := Offset;
    if FFileHeader.LDOffset = 0 then
      FFileHeader.LDOffset := Offset
    else begin
      Stream.Seek(FFileHeader.LDOffset, soFromBeginning);
      if Stream.Read(AHdr,
          SizeOf(TfxRegDataHeader)) <> SizeOf(TfxRegDataHeader) then
        raise EfxRegFileError.Create(SRegfileCorruptError);
      if not AHdr.Deleted then
        raise EfxRegFileError.Create(SRegfileCorruptError)
      else
        AHdr.NDOffset := Offset;
      Stream.Seek(FFileHeader.LDOffset, soFromBeginning);
      if Stream.Write(AHdr,
          SizeOf(TfxRegDataHeader)) <> SizeOf(TfxRegDataHeader) then
        raise EfxRegFileError.Create(SRegfileCorruptError);
      FFileHeader.LDOffset := Offset;
      WriteFileHeader;
    end;
  finally
    Stream.Position := APos;
  end;
end;


procedure TfxRegFile.InternalLoadIndex;
var
  ReadTotal, Count,
  IndexTotal, I, J: integer;
  APos, CIndexOffset: integer;
  pBuf: Pointer;
begin
  FRecOffsetList.Clear;
  if FFileHeader.IndexCount > 0 then
  begin
    APos := Stream.Position;
    try
      IndexTotal := (FFileHeader.IndexCount div fxIndexBlock);
      if (FFileHeader.IndexCount mod fxIndexBlock) > 0 then Inc(IndexTotal);
      for I := 0 to IndexTotal-1 do
      begin
        if (i = (IndexTotal-1)) and
           ((FFileHeader.IndexCount mod fxIndexBlock) > 0) then
          ReadTotal := (FFileHeader.IndexCount mod fxIndexBlock)
        else
          ReadTotal := fxIndexBlock;
        Stream.Position := FFileHeader.IndexOffset + (SizeOf(Integer) * i);
        Count := Stream.Read(CIndexOffset, SizeOf(Integer));
        if (Count <> SizeOf(Integer)) then
          raise EfxRegFileError.Create(SFileReadError);
        if CIndexOffset > 0 then
        begin
          Stream.Position := CIndexOffset;
          GetMem(pBuf, ReadTotal * SizeOf(Integer));
          try
            Count := Stream.Read(pBuf^, ReadTotal * SizeOf(Integer));
            if (Count <> (ReadTotal * SizeOf(Integer))) then
              raise EfxRegFileError.Create(SFileReadError);
            for J := 0 to ReadTotal-1 do
              if (PfxMaxIntArray(pBuf)[J] <> 0) then
                FRecOffsetList.Add(Pointer(PfxMaxIntArray(pBuf)[J]))
              else
                raise EfxRegFileError.Create(SIndexFileCorruptError);
          finally
            FreeMem(pBuf, ReadTotal * SizeOf(Integer));
          end;
        end;
      end;
    finally
      Stream.Position := APos;
    end;
  end;
end;

procedure TfxRegFile.InternalUpdateIndex(AFromIndex: integer);
var
  pBuf: Pointer;
  IndexTotal, Count,
  StartPos, EndPos,
  FromBlock, I, J: integer;
  IndexOffset: Integer;
  APos: integer;
begin
  APos := Stream.Position;
  try
    IndexTotal := (FFileHeader.IndexCount div fxIndexBlock);
    if (FFileHeader.IndexCount mod fxIndexBlock) > 0 then Inc(IndexTotal);

    FromBlock := (AFromIndex div fxIndexBlock);
    for I := FromBlock to (IndexTotal-1) do
    begin
      StartPos := IfThen(I <= FromBlock,
                              AFromIndex,
                              (I * fxIndexBlock));
      EndPos   := (StartPos + fxIndexBlock) - (StartPos mod fxIndexBlock);
      if (EndPos > FFileHeader.IndexCount) or (EndPos = 0) then
        EndPos := FFileHeader.IndexCount;
      Count := EndPos - StartPos;
      GetMem(pBuf, Count * SizeOf(Integer));
      try
        for J := StartPos to EndPos-1 do
          PfxMaxIntArray(pBuf)[j-StartPos] := Integer(FRecOffsetList.Items[j]);
        IndexOffset := GetIndexOffset(StartPos);

        if (StartPos > (I * fxIndexBlock)) then
          IndexOffset := IndexOffset +
                         ((StartPos mod fxIndexBlock) * SizeOf(Integer));
        Stream.Seek(IndexOffset, soFromBeginning);
        Stream.Write(pBuf^, Count * SizeOf(Integer));
      finally
        FreeMem(pBuf, Count * SizeOf(Integer));
      end;
    end;
  finally
    Stream.Position := APos;
  end;
end;

procedure TfxRegFile.ParseKeys(AKeys: string; AStrings: TStringList);
var
  p: integer;
  s: string;
begin
  repeat
    p := Pos('\',AKeys);
    if p>0 then
    begin
      s := copy(AKeys,1,p-1);
      if ((pinteger(@pchar(s)[-4])^) > 0) then
        AStrings.Add(s);
      AKeys := copy(AKeys,p+1,(pinteger(@pchar(AKeys)[-4])^)-p);
    end;
  until p=0;
  if (pinteger(@pchar(AKeys)[-4])^ > 0) then
    AStrings.Add(AKeys);
end;

procedure TfxRegFile.ReadFileHeader;
var
  APos: Integer;
  ACount: Integer;
begin
  APos := Stream.Position;
  Stream.Seek(0, soFromBeginning);
  ACount := Stream.Read(FFileHeader, SizeOf(TfxRegFileHeader));
  try
    if (ACount <> SizeOf(TfxRegFileHeader)) or
       (FFileHeader.Ident <> fxIdent) or
       (FFileHeader.FileVer <> fxFileVer) then
      raise EfxRegFileError.Create(SInvalidFileFormat);
  finally
    Stream.Position := APos;
  end;
end;

procedure TfxRegFile.Reindex;
begin
  InternalLoadIndex;
  FRecOffsetList.Reindex(DefaultIndexSort);
  InternalUpdateIndex(0);
  WriteFileHeader;
end;

procedure TfxRegFile.WriteFileHeader;
var
  APos: Integer;
  ACount: Integer;
begin
  APos := Stream.Position;
  Stream.Seek(0, soFromBeginning);
  ACount := Stream.Write(FFileHeader, SizeOf(TfxRegFileHeader));
  try
    if (ACount <> SizeOf(TfxRegFileHeader)) then
      raise EfxRegFileError.Create(SFileWriteError);
  finally
    Stream.Position := APos;
  end;
end;

procedure TfxRegFile.ReadRecordData(const AOffset: Integer;
  Buffer: TStream);
var
  AHdr: TfxRegDataHeader;
  pBuf: pointer;
  APos: Integer;
begin
  APos := Stream.Position;
  try
    Buffer.Seek(0, soFromBeginning);
    Stream.Seek(AOffset, soFromBeginning);
    if Stream.Read(AHdr, SizeOf(TfxRegDataHeader)) <>
       SizeOf(TfxRegDataHeader) then
      raise EfxRegFileError.Create(SFileReadError);
    Buffer.Write(AHdr, SizeOf(TfxRegDataHeader));
    if (AHdr.DataOffset > 0) and
       (Integer(AHdr.DataType) <= 3) then
    begin
      GetMem(pBuf, AHdr.DataSize);
      try
        Stream.Read(pBuf^, AHdr.DataSize);
        Buffer.Write(pBuf^, AHdr.DataSize);
        Buffer.Size := (AHdr.DataSize + SizeOf(TfxRegDataHeader));
      finally
        FreeMem(pBuf, AHdr.DataSize);
      end;
    end;
  finally
    Stream.Seek(APos, soFromBeginning);
  end;
end;

procedure TfxRegFile.WriteRecordData(Buffer: TStream);
var
  AHdr: TfxRegDataHeader;
  pBuf: pointer;
  APos: Integer;
begin
  pBuf := nil;
  APos := Stream.Position;
  try
    Buffer.Seek(0, soFromBeginning);
    if Buffer.Read(AHdr, SizeOf(TfxRegDataHeader)) <>
       SizeOf(TfxRegDataHeader) then
      raise EfxRegFileError.Create(SFileReadError);
    if (Integer(AHdr.DataType) <= 3) and (AHdr.DataSize > 0) then
    begin
      GetMem(pBuf, AHdr.DataSize);
      Buffer.Read(pBuf^, AHdr.DataSize);
    end;
    InternalCreateDataHeader(@AHdr, pBuf);
  finally
    if pBuf <> nil then
      FreeMem(pBuf, AHdr.DataSize);
    Stream.Seek(APos, soFromBeginning);
  end;
end;

function TfxRegFile.GetParentKeyName: string;
begin
  Result := FPDataHeader.Ident;
end;

function TfxRegFile.OpenKey(const AKeyNames: string;
  const CanCreate: boolean): boolean;
var
  AList: TStrings;
  i: integer;
begin
  Result := True;
  FKeyTree := '';
  if (CompareStr(CurrentKeyName, AKeyNames) <> 0) then
  begin
    AList := TStringList.Create;
    try
      ParseKeys(AKeyNames, TStringList(AList));
      if (Pos('\', AKeyNames) = 1) or
         (FPDataHeader.ID < 1) then
      begin
        InternalReadRootKey;
        FKeyTree := '\';
      end;
      if AList.Count > 0 then
      for i := 0 to AList.Count-1 do
      begin
        Result := InternalOpenKey(AList[i], CanCreate);
        if (not Result) then
          Break;
        if FKeyTree = '\' then
          FKeyTree := FKeyTree+AList[i]
        else
          FKeyTree := FKeyTree+'\'+AList[i];
      end;
    finally
      AList.Free;
    end;
  end;
end;

procedure TfxRegFile.InternalReadRootKey;
var
  AHdr: TfxRegDataHeader;
  APos, Count: Integer;
begin
  APos := Stream.Position;
  try
    { First Data offset assigned in File Header is always our root key }
    Stream.Position := FFileHeader.DataOffset;
    Count := Stream.Read(AHdr, SizeOf(TfxRegDataHeader));
    if Count <> SizeOf(TfxRegDataHeader) then
      raise EfxRegFileError.Create(SFileReadError);
    FastMove(AHdr, FPDataHeader, SizeOf(TfxRegDataHeader));
    FPDataOffset := FFileHeader.DataOffset;
  finally
    Stream.Position := APos;
    FKeyTree := '\';
  end;
end;

function TfxRegFile.InternalOpenKey(const AKeyName: string;
  const CanCreate: boolean): boolean;
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  FRecFound := InternalFind(AKeyName, rtKey, AHdr, DefaultFindCompare);
  { if not found }
  if (FRecFound.Found <> 0) then
  begin
    if CanCreate then
      InternalCreateKey(AKeyName);
  end else
  begin
    FPDataOffset := Integer(FRecOffsetList.Items[FRecFound.LastPos]);
    FastMove(AHdr, FPDataHeader, SizeOf(TfxRegDataHeader));
  end;
  Result := (FRecFound.Found = 0) or CanCreate;
end;

function TfxRegFile.DefaultIndexSort(Item1, Item2: Pointer): Integer;
var
  AHdr1, AHdr2: TfxRegDataHeader;
begin
  AHdr1  := GetRecordHeader(Integer(Item1));
  AHdr2  := GetRecordHeader(Integer(Item2));
  Result := DefFullCompare(@AHdr1, @AHdr2);
{
  Result := CompareValue(AHdr1.PID, AHdr2.PID);
  if Result = 0 then
  begin
    Result := CompareValue(Integer(AHdr1.RegType), Integer(AHdr2.RegType));
    if Result = 0 then
    begin
      Result := CompareStr(AHdr1.Ident, AHdr2.Ident);
      if Result = 0 then
        Result := CompareValue(AHdr1.ID, AHdr2.ID);
    end;
  end;
}
end;

function TfxRegFile.DefaultFindCompare(Item1, Item2: Pointer): Integer;
var
  CompRes: integer;
begin
  CompRes := CompareValue(TfxRegDataHeader(Item1^).ID,
                    TfxRegDataHeader(Item2^).PID);
  if CompRes = 0 then
  begin
    CompRes := CompareValue(Integer(TfxRegDataHeader(Item1^).RegType),
                  Integer(TfxRegDataHeader(Item2^).RegType));
    if CompRes = 0 then
      CompRes := CompareStr(TfxRegDataHeader(Item1^).Ident,
                            TfxRegDataHeader(Item2^).Ident);
  end;
  Result := CompRes;
end;

function TfxRegFile.DefFullCompare(Item1, Item2: Pointer): Integer;
var
  CompRes: integer;
begin
  CompRes := CompareValue(TfxRegDataHeader(Item1^).PID,
                    TfxRegDataHeader(Item2^).PID);
  if CompRes = 0 then
  begin
    CompRes := CompareValue(Integer(TfxRegDataHeader(Item1^).RegType),
                    Integer(TfxRegDataHeader(Item2^).RegType));
    if Compres = 0 then
    begin
      CompRes := CompareStr(TfxRegDataHeader(Item1^).Ident,
                              TfxRegDataHeader(Item2^).Ident);
      if CompRes = 0 then
          CompRes := CompareValue(Integer(TfxRegDataHeader(Item1^).ID),
                Integer(TfxRegDataHeader(Item2^).ID));
    end;
  end;
  Result := CompRes;
end;

function TfxRegFile.DefaultListFindCompare(Item1,
  Item2: Pointer): Integer;
var
  CompRes: integer;
begin
  CompRes := CompareValue(TfxRegDataHeader(Item1^).PID,
                    TfxRegDataHeader(Item2^).PID);
  if CompRes = 0 then
    CompRes := CompareValue(Integer(TfxRegDataHeader(Item1^).RegType),
                  Integer(TfxRegDataHeader(Item2^).RegType));
  Result := CompRes;
end;

{ Special treatment for moveup key }
function TfxRegFile.DefaultCompareID(Item1, Item2: Pointer): Integer;
var
  CompRes: integer;
begin
  CompRes := CompareValue(TfxRegDataHeader(Item1^).PID,
                TfxRegDataHeader(Item2^).PID);
  if CompRes <> 0 then
  begin
    CompRes := CompareValue(Integer(TfxRegDataHeader(Item1^).RegType),
                    Integer(TfxRegDataHeader(Item2^).RegType));
    if Compres = 0 then
    begin
      CompRes := CompareStr(TfxRegDataHeader(Item1^).Ident,
                              TfxRegDataHeader(Item2^).Ident);
      if CompRes <> 0 then
          CompRes := CompareValue(Integer(TfxRegDataHeader(Item1^).PID),
                Integer(TfxRegDataHeader(Item2^).ID));
    end;
  end else
  if CompRes = 0 then
    CompRes := -1;
  Result := CompRes;
end;

function TfxRegFile.InternalFind(const AKeyName: string;
  const AKeyType: TfxRegType; var pHdr: TfxRegDataHeader;
  SCompare: TfxListSortCompare): TfxFindRec;
var
  CompRes, Hi, Lo, Mid, Offset, APos, AIndex: Integer;
  ACount: integer;
  BHdr, AHdr: TfxRegDataHeader;
begin
  FillChar(Result, SizeOf(TfxFindRec), -1);
  CompRes := -1;
  if FRecOffsetList.Count > 0 then
  begin
    APos := Stream.Position;
    try
      Mid:= 0;
      Lo := 0;
      Hi := FRecOffsetList.Count-1;
      FastFillChar(BHdr, SizeOf(TfxRegDataHeader), 0);
      StrPCopy(BHdr.Ident, AKeyName);
      BHdr.ID := FPDataHeader.ID;
      BHdr.PID := FPDataHeader.PID;
      BHdr.RegType := AKeyType;
      while Lo <= Hi do
      begin
        Mid := (Lo + Hi) div 2;
        Offset := Integer(FRecOffsetList.Items[Mid]);
        { cari dulu dalam cache, ketemu gak? }
        AIndex := FRegCacheManager.FindOffset(Offset);
        if AIndex <> -1 then
          AHdr := FRegCacheManager.GetItem(AIndex)
        else begin
          { oh gak ketemu, ok ambil data header tsb dari disk  }
          Stream.Seek(Offset, soFromBeginning);
          ACount := Stream.Read(AHdr, SizeOf(TfxRegDataHeader));
          if (ACount <> SizeOf(TfxRegDataHeader)) then
            raise EfxRegFileError.Create(SFileReadError);
          { push sementara kedalam cache biar kl diperlukan lg
            gak perlu akses disk lg }
          FRegCacheManager.Push(Offset, AHdr);
        end;
        { implementasi aktual b-seach }
        CompRes := SCompare(@BHdr, @AHdr);
        if CompRes > 0 then
          Lo := Mid + 1
        else if CompRes < 0 then
          Hi := Mid - 1
        else
          Break;
      end;
      FastMove(AHdr, pHdr, SizeOf(TfxRegDataHeader));
      Result.Found   := CompRes;
      Result.LastPos := IfThen((CompRes <> 0), Lo, Mid);
      if Result.LastPos >= FRecOffsetList.Count then
        Result.LastPos := FRecOffsetList.Count-1;
    finally
      Stream.Position := APos;
    end;
  end;
end;

function TfxRegFile.InternalFindFirst(const APID: integer;
  const AKeyType: TfxRegType; var pHdr: TfxRegDataHeader): TfxFindRec;
var
  CompRes, Hi, Lo, Mid, Offset, APos: Integer;
  AIndex, ACount: integer;
  AHdr, BHdr: TfxRegDataHeader;
  AFlag: integer;
begin
  FillChar(Result, SizeOf(TfxFindRec), -1);
  CompRes := -1;
  AFlag   := 0;
  if FRecOffsetList.Count > 0 then
  begin
    FastFillChar(BHdr, SizeOf(TfxRegDataHeader), 0);
    BHdr.PID := APID;
    BHdr.RegType := AKeyType;
    APos := Stream.Position;
    try
      Mid:= 0;
      Lo := 0;
      Hi := FRecOffsetList.Count-1;
      while Lo <= Hi do
      begin
        if AFlag = 0 then Mid := (Lo + Hi) div 2 // do bsearch
        else Mid := Mid + AFlag;    // rewind
        if (Mid < 0) then Mid := 0;
        if (Mid >= FRecOffsetList.Count) then Mid := FRecOffsetList.Count-1;
        Offset := Integer(FRecOffsetList.Items[Mid]);
        AIndex := FRegCacheManager.FindOffset(Offset);
        if AIndex <> -1 then
          AHdr := FRegCacheManager.Items[AIndex]
        else begin
          Stream.Seek(Offset, soFromBeginning);
          ACount := Stream.Read(AHdr, SizeOf(TfxRegDataHeader));
          if (ACount <> SizeOf(TfxRegDataHeader)) then
            raise EfxRegFileError.Create(SFileReadError);
          FRegCacheManager.Push(Offset, AHdr);
        end;

        CompRes := DefaultListFindCompare(@BHdr, @AHdr);
        if CompRes = 0 then
        begin
          if AFlag = 0 then // Rewind it...
            AFlag := IfThen((Mid > Lo), -1, 1);
          if AFlag = 1 then  // we found the 1st record
          begin
            FastMove(AHdr, pHdr, SizeOf(TfxRegDataHeader));
            Break;
          end;
        end else
        begin
          if AFlag = -1 then AFlag := 1 // now move 1 record
          else begin
            if CompRes > 0 then Lo := Mid + 1
            else if CompRes < 0 then Hi := Mid - 1
          end;
        end;
      end;
      Result.Found   := CompRes;
      Result.LastPos := IfThen((CompRes <> 0), Lo, Mid);
      Result.PID     := APID;
      Result.RegType := AKeyType;
    finally
      Stream.Position := APos;
    end;
  end;
end;

function TfxRegFile.InternalFindNext(var FindRec: TfxFindRec;
  var pHdr: TfxRegDataHeader): boolean;
var
  Offset, APos, AIndex: Integer;
  Found: boolean;
  AHdr: TfxRegDataHeader;
begin
  APos   := FindRec.LastPos;
  Found  := False;
  while not Found do
  begin
    if APos >= FRecOffsetList.Count then
        break;
    Offset := Integer(FRecOffsetList.Items[APos]);
    AIndex := FRegCacheManager.FindOffset(Offset);
    if AIndex <> -1 then
      AHdr := FRegCacheManager.Items[AIndex]
    else begin
      Stream.Seek(Offset, soFromBeginning);
      if (Stream.Read(AHdr, SizeOf(TfxRegDataHeader)) <>
          SizeOf(TfxRegDataHeader)) then
        raise EfxRegFileError.Create(SFileReadError);
      FRegCacheManager.Push(Offset, AHdr);
    end;
    Found  := (CompareValue(FindRec.PID, AHdr.PID) = 0);
    if Found then
    begin
      Found := (CompareValue(Integer(FindRec.RegType),
        Integer(AHdr.RegType)) = 0);
      if Found then
      begin
        FastMove(AHdr, pHdr, SizeOf(TfxRegDataHeader));
        Inc(APos);
      end else
         Inc(APos);
    end else
      break;
  end;
  FindRec.Found  := Integer(Found);
  FindRec.LastPos:= APos;
  Result := Found;
end;

function TfxRegFile.HasSubKeys: Boolean;
var
  AHdr: TfxRegDataHeader;
begin
  Result := (InternalFindFirst(FPDataHeader.ID, rtKey, AHdr).Found = 0);
end;

function TfxRegFile.KeyExists(const Key: string): Boolean;
var
  AHdr: TfxRegDataHeader;
begin
  Result := (InternalFind(Key, rtKey, AHdr, DefaultFindCompare).Found = 0);
end;

function TfxRegFile.ValueExists(const Name: string): Boolean;
var
  AHdr: TfxRegDataHeader;
begin
  Result := (InternalFind(Name, rtValue, AHdr, DefaultFindCompare).Found = 0);
end;

procedure TfxRegFile.MoveUpKey;
var
  APID, I, Offset, APos, AIndex: Integer;
  ACount: integer;
  AHdr: TfxRegDataHeader;
begin
  if FRecOffsetList.Count > 0 then
  begin
    APos := Stream.Position;
    try
      APID := FPDataHeader.PID;
      for I := 0 to FRecOffsetList.Count-1 do
      begin
        Offset := Integer(FRecOffsetList.Items[I]);
        { cari dulu dalam cache, ketemu gak? }
        AIndex := FRegCacheManager.FindOffset(Offset);
        if AIndex <> -1 then
          AHdr := FRegCacheManager.GetItem(AIndex)
        else begin
          { oh gak ketemu, ok ambil data header tsb dari disk  }
          Stream.Seek(Offset, soFromBeginning);
          ACount := Stream.Read(AHdr, SizeOf(TfxRegDataHeader));
          if (ACount <> SizeOf(TfxRegDataHeader)) then
            raise EfxRegFileError.Create(SFileReadError);
          { push sementara kedalam cache biar kl diperlukan lg
            gak perlu akses disk lg }
          FRegCacheManager.Push(Offset, AHdr);
        end;
        if AHdr.ID = APID then
        begin
          FPDataOffset := Integer(FRecOffsetList.Items[I]);
          FastMove(AHdr, FPDataHeader, SizeOf(TfxRegDataHeader));
          break;
        end;
      end;
    finally
      Stream.Position := APos;
    end;
  end;
end;
  
procedure TfxRegFile.CloseKey;
begin
  InternalReadRootKey;
end;

procedure TfxRegFile.GetKeyNames(Strings: TStrings);
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  Strings.Clear;
  FRecFound := InternalFindFirst(CurrentParentID, rtKey,AHdr);
  if (FRecFound.Found = 0) then
  while InternalFindNext(FRecFound, AHdr) do
    Strings.Add(AHdr.Ident);
end;

procedure TfxRegFile.GetValueNames(Strings: TStrings);
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  Strings.Clear;
  FRecFound := InternalFindFirst(CurrentParentID, rtValue,AHdr);
  if (FRecFound.Found = 0) then
  while InternalFindNext(FRecFound, AHdr) do
    Strings.Add(AHdr.Ident);
end;

procedure TfxRegFile.DeleteKey(const Key: string);
var
  AList: TStrings;
  APHeader: TfxRegDataHeader;
  AOffset: Integer;
  AHdr: TfxRegDataHeader;
  AFindRec: TfxFindRec;
  AKey: TfxIdentName;
begin
  FastMove(FPDataHeader, APHeader, SizeOf(TfxRegDataHeader));
  AList := TStringList.Create;
  if OpenKey(Key) then
  try
    repeat
      GetValueNames(AList);
      if AList.Count > 0 then
        while AList.Count > 0 do
        begin
          DeleteValue(AList[0]);
          AList.Delete(0);
        end;
      GetKeyNames(AList);
      if AList.Count > 0 then
        while AList.Count > 0 do
        begin
          DeleteKey(AList[0]);
          AList.Delete(0);
        end;
      StrPCopy(AKey, FPDataHeader.Ident);
      MoveUpKey;
      FastFillChar(AHdr, SizeOf(TfxRegDataHeader), 0);
      AFindRec := InternalFind(AKey, rtKey, AHdr, DefaultFindCompare);
      if (AFindRec.Found = 0) then
      begin
        AOffset := Integer(FRecOffsetList.Items[AFindRec.LastPos]);
        InternalDelete(AOffset, @AHdr);
        FRecOffsetList.Delete(AFindRec.LastPos);
        if FRegCacheManager.FindOffset(AOffset) = 0 then
          FRegCacheManager.Delete(0);
        Dec(FFileHeader.IndexCount);
        InternalUpdateIndex(AFindRec.LastPos);
        WriteFileHeader;
      end;
    until (CompareMem(@APHeader, @FPDataHeader,
            SizeOf(TfxRegDataHeader)) = True) or
          (StrComp(APHeader.Ident, FPDataHeader.Ident) = 0);
  finally
    AList.Free;
  end;
end;

procedure TfxRegFile.DeleteValue(const Name: string);
var
  AOffset: Integer;
  AHdr: TfxRegDataHeader;
  AFindRec: TfxFindRec;
begin
  FastFillChar(AHdr, SizeOf(TfxRegDataHeader), 0);
  AFindRec := InternalFind(Name, rtValue, AHdr, DefaultFindCompare);
  if (AFindRec.Found = 0) then
  begin
    AOffset := Integer(FRecOffsetList.Items[AFindRec.LastPos]);
    InternalDelete(AOffset, @AHdr);
    FRecOffsetList.Delete(AFindRec.LastPos);
    if FRegCacheManager.FindOffset(AOffset) = 0 then
      FRegCacheManager.Delete(0);
    Dec(FFileHeader.IndexCount);
    InternalUpdateIndex(AFindRec.LastPos);
    WriteFileHeader;
  end;
end;

function TfxRegFile.FindInsertPos(const DataType, ReqSize: integer): integer;
var
  AHdr, BHdr: TfxRegDataHeader;
  APos, BPos: Integer;
  Found: boolean;
begin
  Result := 0;
  APos := FFileHeader.FDOffset;
  BPos := 0;
  if APos > 0 then
  begin
    Found := False;
    while not Found do
    begin
      Stream.Seek(APos, soFromBeginning);
      if Stream.Read(AHdr, SizeOf(TfxRegDataHeader)) <>
          SizeOf(TfxRegDataHeader) then
        raise EfxRegFileError.Create(SRegfileCorruptError);
      if DataType > 3 then
      begin
        FFileHeader.FDOffset := IfThen(FFileHeader.FDOffset = APos,
                                  AHdr.NDOffset,
                                  FFileHeader.FDOffset);
        FFileHeader.LDOffset := IfThen(FFileHeader.LDOffset = APos,
                                  FFileHeader.FDOffset,
                                  FFileHeader.LDOffset);
        WriteFileHeader;
        Found := True;
        Result := APos;
      end else
      begin
        if (Integer(AHdr.DataType) <= 3) and
           (AHdr.DataSize >= ReqSize) then
        begin
          FFileHeader.FDOffset := IfThen(FFileHeader.FDOffset = APos,
                                  AHdr.NDOffset,
                                  FFileHeader.FDOffset);
          FFileHeader.LDOffset := IfThen(FFileHeader.LDOffset = APos,
                                    Ifthen(BPos <> 0,
                                      BPos,
                                      FFileHeader.FDOffset),
                                    FFileHeader.LDOffset);
          if BPos <> 0 then
          begin
            Stream.Seek(BPos, soFromBeginning);
            if Stream.Read(BHdr, SizeOf(TfxRegDataHeader)) <>
                SizeOf(TfxRegDataHeader) then
                  raise EfxRegFileError.Create(SRegfileCorruptError);
            BHdr.NDOffset := AHdr.NDOffset;
            Stream.Seek(BPos, soFromBeginning);
            if Stream.Write(BHdr, SizeOf(TfxRegDataHeader)) <>
                SizeOf(TfxRegDataHeader) then
                  raise EfxRegFileError.Create(SRegfileCorruptError);

          end;
          WriteFileHeader;
          Found := True;
          Result := APos;
        end else
        if APos = AHdr.NDOffset then
        begin
            Found := True;
            Result:= AHdr.NDOffset;
        end else
        begin
          BPos := APos;
          APos := AHdr.NDOffset;
        end;
      end;
      if (APos = 0) then
      begin
        Found := True;
        Result:= Stream.Seek(0, soFromEnd);
      end;
    end;
  end else
    Result := Stream.Seek(0, soFromEnd);
end;

procedure TfxRegFile.Compact(const TmpFileName: string);
var
  ARegFile: TfxRegFile;
  AHdr: TfxRegDataHeader;
  AStream: TMemoryStream;
  i, ReadCount, TotalSize: integer;
  pBuf: Pointer;
begin
  if FileExists(TmpFileName) then
    DeleteFile(TmpFileName);
  ARegFile := TfxRegFile.Create(TmpFileName);
  try
    if FRecOffsetList.Count > 0 then
    for i := 0 to FRecOffsetList.Count-1 do
    begin
      AStream := TMemoryStream.Create;
      try
        ReadRecordData(Integer(FRecOffsetList.Items[i]), AStream);
        AStream.Seek(0, soFromBeginning);
        if (AStream.Size < SizeOf(TfxRegDataHeader)) or
           (AStream.Read(AHdr, SizeOf(TfxRegDataHeader)) <>
              SizeOf(TfxRegDataHeader)) then
          raise EfxRegFileError.Create(SRegfileCorruptError);
        if CompareStr(AHdr.Ident, fxRootID) <> 0 then
          ARegFile.WriteRecordData(AStream);
      finally
        AStream.Free;
      end;
    end;
    TotalSize := 0;
    Stream.Size := 0;
    Stream.Position := 0;
    FRegCacheManager.Clear;
    FRecOffsetList.Clear;
    ARegFile.Stream.Seek(0, soFromBeginning);
    GetMem(pBuf, fxIndexBlock);
    try
      while True do
      begin
        ReadCount := ARegFile.Stream.Read(pBuf^, fxIndexBlock);
        if ReadCount > 0 then
        begin
          Stream.Write(pBuf^, ReadCount);
          TotalSize := TotalSize + ReadCount;
        end;
        if (ReadCount <> fxIndexBlock) then
          break;
      end;
      Initialize;
      Reindex;
    finally
      FreeMem(pBuf, fxIndexBlock);
      Stream.Size := TotalSize;
    end;
  finally
    ARegFile.Free;
    DeleteFile(TmpFileName);
  end;
end;

procedure TfxRegFile.ReadStream(const Name: string;
  AStream: TStream);
var
  FRecFound : TfxFindRec;
  DataOffset: integer;
  AHdr: TfxRegDataHeader;
  Strm: TMemoryStream;
  pBuf: Pointer;
begin
  FRecFound := InternalFind(Name, rtValue, AHdr, DefaultFindCompare);
  if (FRecFound.Found = 0) then
  begin
    DataOffset := Integer(FRecOffsetList.Items[FRecFound.LastPos]);
    Strm := TMemoryStream.Create;
    try
      ReadRecordData(DataOffset, Strm);
      Strm.Seek(0, soFromBeginning);
      if Strm.Size >= SizeOf(TfxRegDataHeader) then
        Strm.Read(AHdr, SizeOf(TfxRegDataHeader));
      if (AHdr.DataSize > 0) and (Integer(AHdr.DataType) <= 3) then
      begin
        GetMem(pBuf, AHdr.DataSize);
        try
          Strm.Read(pBuf^, AHdr.DataSize);
          AStream.Seek(0, soFromBeginning);
          AStream.Write(pBuf^, AHdr.DataSize);
        finally
          FreeMem(pBuf, AHdr.DataSize);
        end;
      end;
    finally
      Strm.Free;
    end;
  end;
end;

function TfxRegFile.ReadBool(const Name: string): Boolean;
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  FRecFound := InternalFind(Name, rtValue, AHdr, DefaultFindCompare);
  if (FRecFound.Found = 0) then
  begin
    if AHdr.DataType <> rdBoolean then
      raise EfxRegFileError.Create(SDataTypeError);
    FastMove(AHdr.IntData, Result, SizeOf(Boolean));
  end;
end;

function TfxRegFile.ReadCurrency(const Name: string): Currency;
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  FRecFound := InternalFind(Name, rtValue, AHdr, DefaultFindCompare);
  if (FRecFound.Found = 0) then
  begin
    if AHdr.DataType <> rdCurrency then
      raise EfxRegFileError.Create(SDataTypeError);
    FastMove(AHdr.IntData, Result, SizeOf(Currency));
  end;
end;

function TfxRegFile.ReadDate(const Name: string): TDateTime;
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  FRecFound := InternalFind(Name, rtValue, AHdr, DefaultFindCompare);
  if (FRecFound.Found = 0) then
  begin
    if AHdr.DataType <> rdDateTime then
      raise EfxRegFileError.Create(SDataTypeError);
    FastMove(AHdr.IntData, Result, SizeOf(TDateTime));
  end;
end;

function TfxRegFile.ReadDateTime(const Name: string): TDateTime;
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  FRecFound := InternalFind(Name, rtValue, AHdr, DefaultFindCompare);
  if (FRecFound.Found = 0) then
  begin
    if AHdr.DataType <> rdDateTime then
      raise EfxRegFileError.Create(SDataTypeError);
    FastMove(AHdr.IntData, Result, SizeOf(TDateTime));
  end;
end;

function TfxRegFile.ReadFloat(const Name: string): Double;
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  FRecFound := InternalFind(Name, rtValue, AHdr, DefaultFindCompare);
  if (FRecFound.Found = 0) then
  begin
    if AHdr.DataType <> rdDouble then
      raise EfxRegFileError.Create(SDataTypeError);
    FastMove(AHdr.IntData, Result, SizeOf(Double));
  end;
end;

function TfxRegFile.ReadInteger(const Name: string): Integer;
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  FRecFound := InternalFind(Name, rtValue, AHdr, DefaultFindCompare);
  if (FRecFound.Found = 0) then
  begin
    if AHdr.DataType <> rdInteger then
      raise EfxRegFileError.Create(SDataTypeError);
    FastMove(AHdr.IntData, Result, SizeOf(Integer));
  end;
end;

function TfxRegFile.ReadString(const Name: string): string;
var
  AStream: TMemoryStream;
  pBuf  : PChar;
begin
  AStream := TMemoryStream.Create;
  try
    ReadStream(Name, AStream);
    if AStream.Size > 0 then
    begin
      pBuf := StrAlloc(AStream.Size);
      try
       AStream.Seek(0, soFromBeginning);
       AStream.Read(pBuf^, AStream.Size);
       Result := StrPas(pBuf);
      finally
       StrDispose(pBuf);
      end;
    end;
  finally
    AStream.Free;
  end;
end;

procedure TfxRegFile.ReadStrings(const Name: string; List: TStrings);
var
  AStream: TMemoryStream;
begin
  List.Clear;
  AStream := TMemoryStream.Create;
  try
    ReadStream(Name, AStream);
    if AStream.Size > 0 then
      List.LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

function TfxRegFile.ReadTime(const Name: string): TDateTime;
var
  FRecFound : TfxFindRec;
  AHdr: TfxRegDataHeader;
begin
  FRecFound := InternalFind(Name, rtValue, AHdr, DefaultFindCompare);
  if (FRecFound.Found = 0) then
  begin
    if AHdr.DataType <> rdDateTime then
      raise EfxRegFileError.Create(SDataTypeError);
    FastMove(AHdr.IntData, Result, SizeOf(TDateTime));
  end;
end;

function TfxRegFile.GetCurrentParentID: integer;
begin
  Result := FPDataHeader.ID;
end;

procedure TfxRegFile.WriteBinaryData(const Name: string;
  const DataType: TfxRegDataType;Buffer: pointer;
  const BufSize: Integer);
var
  AHdr: TfxRegDataHeader;
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    FastFillChar(AHdr, SizeOf(TfxRegDataHeader), 0);
    StrPCopy(AHdr.Ident, Name);
    AHdr.PID := CurrentParentID;
    AHdr.RegType := rtValue;
    AHdr.DataType:= DataType;
    AHdr.DataSize:= BufSize;
    AHdr.EmptyData := (Buffer = nil);
    AHdr.Deleted := False;
    AStream.Seek(0, soFromBeginning);
    if Integer(DataType) > 3 then
    begin
      FastMove(Buffer^, AHdr.IntData, BufSize);
      AStream.Write(AHdr,SizeOf(TfxRegDataHeader));
    end else
    begin
      AStream.Write(AHdr,SizeOf(TfxRegDataHeader));
      AStream.Write(Buffer^, BufSize);
    end;
    WriteRecordData(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TfxRegFile.WriteBool(const Name: string; Value: Boolean);
begin
  WriteBinaryData(Name, rdBoolean, @Value, SizeOf(Boolean));
end;

procedure TfxRegFile.WriteCurrency(const Name: string; Value: Currency);
begin
  WriteBinaryData(Name, rdCurrency, @Value, SizeOf(Currency));
end;

procedure TfxRegFile.WriteDate(const Name: string; Value: TDateTime);
begin
  WriteBinaryData(Name, rdDateTime, @Value, SizeOf(TDateTime));
end;

procedure TfxRegFile.WriteDateTime(const Name: string; Value: TDateTime);
begin
  WriteBinaryData(Name, rdDateTime, @Value, SizeOf(TDateTime));
end;

procedure TfxRegFile.WriteFloat(const Name: string; Value: Double);
begin
  WriteBinaryData(Name, rdDouble, @Value, SizeOf(Double));
end;

procedure TfxRegFile.WriteInteger(const Name: string; Value: Integer);
begin
  WriteBinaryData(Name, rdInteger, @Value, SizeOf(Integer));
end;

procedure TfxRegFile.WriteStream(const Name: string; Stream: TStream);
var
  pBuf : Pointer;
begin
  GetMem(pBuf, Stream.Size);
  try
   Stream.Seek(0, soFromBeginning);
   Stream.Read(pBuf^, Stream.Size);
   WriteBinaryData(Name, rdBinary, pBuf, Stream.Size);
  finally
   FreeMem(pBuf, Stream.Size);
  end;
end;

procedure TfxRegFile.WriteString(const Name, Value: string);
var
  pBuf : pChar;
begin
  pBuf := StrNew(PChar(Value));
  try
   WriteBinaryData(Name, rdString, pBuf, StrLen(pBuf) + 1);
  finally
   StrDispose(pBuf);
  end;
end;

procedure TfxRegFile.WriteTime(const Name: string; Value: TDateTime);
begin
  WriteBinaryData(Name, rdDateTime, @Value, SizeOf(TDateTime));
end;

function TfxRegFile.GetItemPos(Index: Integer): Integer;
begin
  Result := Integer(FRecOffsetList.Items[Index]);
end;

function TfxRegFile.GetIndexCount: integer;
begin
  Result := FRecOffsetList.Count;
end;

function TfxRegFile.GetDataInfo(const ValueName: string;
  var Value: TfxRegDataInfo): Boolean;
var
  AHdr: TfxRegDataHeader;
begin
  Result := (InternalFind(ValueName, rtValue, AHdr,
                DefaultFindCompare).Found = 0);
  Value.RegData := AHdr.DataType;
  Value.DataSize:= AHdr.DataSize;
end;

function TfxRegFile.GetDataSize(const ValueName: string): Integer;
var
  Value: TfxRegDataInfo;
begin
  if GetDataInfo(ValueName, Value) then
    Result := Value.DataSize else
    Result := -1;
end;

function TfxRegFile.GetDataType(const ValueName: string): TfxRegDataType;
var
  Value: TfxRegDataInfo;
begin
  if GetDataInfo(ValueName, Value) then
    Result := Value.RegData else
    Result := rdUnknown;
end;

procedure TfxRegFile.CreateKey(const Key: string);
begin
  InternalCreateKey(Key);
end;

procedure TfxRegFile.WriteStrings(const Name: string; List: TStrings);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    List.SaveToStream(AStream);
    WriteStream(Name, AStream);
  finally
    AStream.Free;
  end;
end;

procedure TfxRegFile.LoadFromStream(AStream: TStream);
var
  PtrBuf: Pointer;
begin
  Clear;
  Stream.Size := 0;
  AStream.Position := 0;
  if AStream.Size > 0 then
  begin
    Stream.Size := AStream.Size;
    GetMem(PtrBuf, AStream.Size);
    try
      AStream.ReadBuffer(PtrBuf^, AStream.Size);
      Stream.WriteBuffer(PtrBuf^, AStream.Size);      
    finally
      FreeMem(PtrBuf);
    end;
  end;
  Initialize;
end;

procedure TfxRegFile.SaveToStream(AStream: TStream);
var
  PtrBuf: Pointer;
begin
  Stream.Position := 0;
  if Stream.Size > 0 then
  begin
    GetMem(PtrBuf, Stream.Size);
    try
      Stream.ReadBuffer(PtrBuf^, Stream.Size);
      AStream.WriteBuffer(PtrBuf^, Stream.Size);      
    finally
      FreeMem(PtrBuf);
    end;
  end;
end;

procedure TfxRegFile.Compact;
var
  ARegFile: TfxRegFile;
  AHdr: TfxRegDataHeader;
  ZStream, AStream: TMemoryStream;
  i, ReadCount, TotalSize: integer;
  pBuf: Pointer;
begin
  ZStream  := TMemoryStream.Create;
  ARegFile := TfxRegFile.Create(ZStream);
  try
    if FRecOffsetList.Count > 0 then
    for i := 0 to FRecOffsetList.Count-1 do
    begin
      AStream := TMemoryStream.Create;
      try
        ReadRecordData(Integer(FRecOffsetList.Items[i]), AStream);
        AStream.Seek(0, soFromBeginning);
        if (AStream.Size < SizeOf(TfxRegDataHeader)) or
           (AStream.Read(AHdr, SizeOf(TfxRegDataHeader)) <>
              SizeOf(TfxRegDataHeader)) then
          raise EfxRegFileError.Create(SRegfileCorruptError);
        if CompareStr(AHdr.Ident, fxRootID) <> 0 then
          ARegFile.WriteRecordData(AStream);
      finally
        AStream.Free;
      end;
    end;
    TotalSize := 0;
    Stream.Size := 0;
    Stream.Position := 0;
    FRegCacheManager.Clear;
    FRecOffsetList.Clear;
    ARegFile.Stream.Seek(0, soFromBeginning);
    GetMem(pBuf, fxIndexBlock);
    try
      while True do
      begin
        ReadCount := ARegFile.Stream.Read(pBuf^, fxIndexBlock);
        if ReadCount > 0 then
        begin
          Stream.Write(pBuf^, ReadCount);
          TotalSize := TotalSize + ReadCount;
        end;
        if (ReadCount <> fxIndexBlock) then
          break;
      end;
      Initialize;
      Reindex;
    finally
      FreeMem(pBuf, fxIndexBlock);
      Stream.Size := TotalSize;
    end;
  finally
    ARegFile.Free;
    ZStream.Free;
  end;
end;

procedure TfxRegFile.Clear;
begin
  FRegCacheManager.Clear;
  FFileStream.Position := 0;
  FFileStream.Size := 0;
  Initialize;
end;

end.
