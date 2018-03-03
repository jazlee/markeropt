unit NativeRegIniXML;

interface
uses
  SysUtils, Classes, NativeRegXML, IniFiles;

type
  TRegXMLIniFile = class(TCustomIniFile)
  private
    FRegIniFile: TRegXML;
    FBasePath: string;
  public
    constructor Create(const FileName: string); overload;
    constructor Create(const FileName, ABasePath: string); overload;
    destructor Destroy; override;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; override;
    function ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function ReadString(const Section, Ident, Default: string): string; override;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
//    procedure ReadKeys(const Section: string; Sections: TStrings);
    procedure WriteDate(const Section, Name: string; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: string; Value: Double); override;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); overload; override;
    procedure ReadSections(const Section: string; Strings: TStrings); overload; override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;

    property RegIniFile: TRegXML read FRegIniFile;
    property BasePath: string read FBasePath;
  end;

implementation

{ TRegXMLIniFile }

constructor TRegXMLIniFile.Create(const FileName: string);
begin
  Create(FileName, '\');
end;

constructor TRegXMLIniFile.Create(const FileName, ABasePath: string);
begin
  inherited Create(FileName);
  FRegIniFile := TRegXML.Create(nil);
  FRegIniFile.Open(FileName);
  FBasePath := ABasePath;
end;

procedure TRegXMLIniFile.DeleteKey(const Section, Ident: String);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True) then
    FRegIniFile.deleteKey(Ident);
end;

destructor TRegXMLIniFile.Destroy;
begin
  FRegIniFile.Free;
  inherited Destroy;
end;

procedure TRegXMLIniFile.EraseSection(const Section: string);
begin
  FRegIniFile.deleteKey(FBasePath+'\'+Section);
end;

function TRegXMLIniFile.ReadDate(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True) and
     FRegIniFile.valueExists(Name) then
    Result := FRegIniFile.readDate(Name)
  else Result := Default;
end;

function TRegXMLIniFile.ReadDateTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True) and
     FRegIniFile.valueExists(Name) then
    Result := FRegIniFile.readDateTime(Name)
  else Result := Default;
end;

function TRegXMLIniFile.ReadFloat(const Section, Name: string;
  Default: Double): Double;
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True) and
     FRegIniFile.valueExists(Name) then
    Result := FRegIniFile.readFloat(Name)
  else Result := Default;
end;

function TRegXMLIniFile.ReadInteger(const Section, Ident: string;
  Default: Integer): Longint;
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True) and
     FRegIniFile.valueExists(Ident) then
    Result := FRegIniFile.readInteger(Ident)
  else Result := Default;
end;

procedure TRegXMLIniFile.ReadSection(const Section: string; Strings: TStrings);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True)then
    FRegIniFile.getValueNames(Strings);
end;

procedure TRegXMLIniFile.ReadSections(const Section: string; Strings: TStrings);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True) then
    FRegIniFile.getKeyNames(Strings);
end;

procedure TRegXMLIniFile.ReadSections(Strings: TStrings);
begin
  FRegIniFile.getKeyNames(Strings);
end;

procedure TRegXMLIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True) then
    FRegIniFile.getValueNames(Strings);
end;

function TRegXMLIniFile.ReadString(const Section, Ident,
  Default: string): string;
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True) and
     FRegIniFile.valueExists(Ident) then
    Result := FRegIniFile.readString(Ident)
  else Result := Default;
end;

function TRegXMLIniFile.ReadTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True) and
     FRegIniFile.valueExists(Name) then
    Result := FRegIniFile.readTime(Name)
  else Result := Default;
end;

procedure TRegXMLIniFile.UpdateFile;
begin
  inherited;

end;

procedure TRegXMLIniFile.WriteDate(const Section, Name: string;
  Value: TDateTime);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True)then
    FRegIniFile.writeDate(Name, Value);
end;

procedure TRegXMLIniFile.WriteDateTime(const Section, Name: string;
  Value: TDateTime);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True)then
    FRegIniFile.writeDateTime(Name, Value);
end;

procedure TRegXMLIniFile.WriteFloat(const Section, Name: string; Value: Double);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True)then
    FRegIniFile.writeFloat(Name, Value);
end;

procedure TRegXMLIniFile.WriteInteger(const Section, Ident: string;
  Value: Integer);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True)then
    FRegIniFile.writeInteger(Ident, Value);
end;

procedure TRegXMLIniFile.WriteString(const Section, Ident, Value: String);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True)then
    FRegIniFile.writeString(Ident, Value);
end;

procedure TRegXMLIniFile.WriteTime(const Section, Name: string;
  Value: TDateTime);
begin
  if FRegIniFile.openKey(FBasePath+'\'+Section, True)then
    FRegIniFile.writeTime(Name, Value);
end;

end.
