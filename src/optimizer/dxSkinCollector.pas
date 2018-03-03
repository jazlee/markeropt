unit dxSkinCollector;

interface

uses
  SysUtils, Classes, TPropertiesUnit, dxSkinsCore, cxLookAndFeels,
  dxSkinsForm;

type
  TdxSkinCollector = class(TComponent)
  private
    FSkinFolder: string;
    FSkins: TProperties;
    FCount: integer;
    FSkinNames: TStringList;
    FSkinController: TdxSkinController;
    FSkinName: string;
    FUseSkin: boolean;
    function GetSkinNames: TStrings;
    procedure SetSkinName(const Value: string);
    procedure SetUseSkin(const Value: boolean);
  protected
    procedure GetSkinInfo(const ASkinFile: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Refresh;

    procedure LoadSkin(const ASkinName: string);

    property SkinName: string read FSkinName write SetSkinName;
    property UseSkin: boolean read FUseSkin write SetUseSkin;

    property SkinNames: TStrings read GetSkinNames;
    property SkinFolder: string read FSkinFolder write FSkinFolder;
    property SkinCount: integer read FCount;
    property SkinController: TdxSkinController read FSkinController write FSkinController;  
  end;

implementation
uses
  cspapputil, dxSkinsDefaultPainters;

function ReadStringFromStream(AStream: TStream): string;
var
  L: Integer;
begin
  AStream.Read(L, SizeOf(L));
  SetLength(Result, L);
  if L > 0 then
    AStream.ReadBuffer(Result[1], L);
end;  

{ TdxSkinCollector }

procedure TdxSkinCollector.Clear;
begin
  FSkins.Clear;
  FSkinNames.Clear;
  FCount := 0;
end;

constructor TdxSkinCollector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSkinFolder := GetAppIDClass.ApplicationPath+'\skins\';
  FSkinNames := TStringList.Create;
  FSkins := TProperties.Create;
  FCount := 0;
end;

destructor TdxSkinCollector.Destroy;
begin
  FSkins.Free;
  FSkinNames.Free;
  inherited Destroy;
end;

procedure TdxSkinCollector.GetSkinInfo(const ASkinFile: string);
var
  AStream: TFileStream;
  AVersion: Double;
  ASkinCount: Integer;
  ASkinSize: Integer;
  ASavedPosition: Int64;
  I: Integer;
  ASkinName: String;
begin
  if FileExists(ASkinFile) then
  begin
    AStream := TFileStream.Create(ASkinFile, fmOpenRead or fmShareDenyNone);
    try
      if dxSkinCheckSignature(AStream, AVersion) and
         (AStream.Read(ASkinCount, SizeOf(ASkinCount)) = SizeOf(ASkinCount)) then
      begin
        for I := 0 to ASkinCount - 1 do
        begin
          AStream.Read(ASkinSize, SizeOf(ASkinSize));
          ASavedPosition := AStream.Position;
          if (not dxSkinCheckSignature(AStream, AVersion)) then
            break;
          ASkinName := ReadStringFromStream(AStream);
          if ASkinName <> EmptyStr then
            FSkins.SetProperty(ASkinName, ASkinFile);
          AStream.Position := ASavedPosition + ASkinSize;
        end;
      end;           
    finally
      AStream.Free;
    end;
  end;
end;

function TdxSkinCollector.GetSkinNames: TStrings;
begin
  Result := FSkinNames;
end;

procedure TdxSkinCollector.LoadSkin(const ASkinName: string);
var
  AFile: string;
begin
  AFile := FSkins.GetProperty(ASkinName);
  if (AFile <> EmptyStr) and (FileExists(AFile)) then
    dxSkinsUserSkinLoadFromFile(AFile, ASkinName);
end;

procedure TdxSkinCollector.Refresh;
var
  ASearchRec: TSearchRec;
  nFileAttr: integer;
  AModPath: string;
  APath, AFile: string;
  AStrList: TStrings;
begin
  Clear;
  APath := SkinFolder;
  AModPath := APath + '*.skinres';
  nFileAttr := faHidden + faSysFile + faAnyFile;
  if FindFirst(AModPath, nFileAttr, ASearchRec) = 0 then
  begin
    try
      repeat
        AFile := APath + ASearchRec.Name;
        if FileExists(AFile) then
        begin
          try
            GetSkinInfo(AFile);
          except; end;
        end;
      until (FindNext(ASearchRec) <> 0);
      AStrList := FSkins.GetPropertyNames;
      try
        FSkinNames.Assign(AStrList);
        FCount := FSkinNames.Count;
      finally
        AStrList.Free;
      end;
    finally
      SysUtils.FindClose(ASearchRec)
    end;
  end;
end;

procedure TdxSkinCollector.SetSkinName(const Value: string);
begin
  if (FSkinName <> Value) or (FUseSkin) then
  begin
    FSkinName := Value;
    FUseSkin := True;    
    LoadSkin(FSkinName);
    FSkinController.SkinName := sdxSkinsUserSkinName;
    FSkinController.UseSkins := FUseSkin;
  end;
end;

procedure TdxSkinCollector.SetUseSkin(const Value: boolean);
begin
  if FUseSkin <> Value then
  begin
    FUseSkin := Value;
    FSkinController.UseSkins := FUseSkin;
    if not FUseSkin then
      FSkinController.Kind := lfOffice11;    
  end;
end;

end.
