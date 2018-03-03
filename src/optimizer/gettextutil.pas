unit gettextutil;

interface

uses
  SysUtils, Classes, Windows, gnugettext;

type

  TIdentifierCharset = set of Char;

  TPOParser = class(TObject)
  private
    FCommentLine: String;
    FKeywords: TStrings;
    FText: String;
    FPosition: Integer;
    FIdentifierCharset: TIdentifierCharset;
    FSkipSpace: Boolean;
    FSkipEOL: Boolean;
    FStringQuotes: String;
    FSkipChar: String;
    FSize: Integer;
    FLastPosition: Integer;
    FUseY: Boolean;
    FYList: TList;
    procedure SetPosition(const Value: Integer);
    procedure SetText(const Value: String);
    function Ident: String;
    function DoDigitSequence: Boolean;
    function DoUnsignedInteger: Boolean;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ConstructCharset(const s: String);

    { skip all #0..#31 symbols }
    procedure SkipSpaces;
    { get EOL symbol }
    function GetEOL: Boolean;
    { get any valid ident except keyword }
    function GetIdent: String;
    { get any valid punctuation symbol like ,.;: }
    function GetChar: String;
    { get any valid ident or keyword }
    function GetWord: String;
    { get valid quoted/control string like 'It''s'#13#10'working' }
    function GetString: String;
    { get Y:X position }
    function GetXYPosition: String;
    function GetPlainPosition(pt: TPoint): Integer;
    { is this keyword? }
    function IsKeyWord(const s: String): Boolean;
    
    property CommentLine: String read FCommentLine write FCommentLine;
    property Keywords: TStrings read FKeywords;
    property IdentifierCharset: TIdentifierCharset read FIdentifierCharset
      write FIdentifierCharset;
    property SkipChar: String read FSkipChar write FSkipChar;
    property SkipEOL: Boolean read FSkipEOL write FSkipEOL;
    property SkipSpace: Boolean read FSkipSpace write FSkipSpace;
    property StringQuotes: String read FStringQuotes write FStringQuotes;

    property Position: Integer read FPosition write SetPosition;
    property Text: String read FText write SetText;    
  end;

  TTextItem = class(TCollectionItem)
  private
    FAutoComments: AnsiString;
    FComments: AnsiString;
    FReferences: AnsiString;
    FMsgStr: AnsiString;
    FMsgId: AnsiString;
    FFlags: AnsiString;
    function GetTranslated: boolean;
  published
    property Comments: AnsiString read FComments write FComments;
    property AutoComments: AnsiString read FAutoComments write FAutoComments;
    property References: AnsiString read FReferences write FReferences;
    property Flags: AnsiString read FFlags write FFlags;
    property MsgId: AnsiString read FMsgId write FMsgId;
    property MsgStr: AnsiString read FMsgStr write FMsgStr;

    property Translated: boolean read GetTranslated;
  end;

  TTextDomain = class(TOwnedCollection)
  private
  protected
  public
    constructor Create(AOwner: TPersistent);

    function AddText: TTextItem;
  end;

implementation

{ TTextItem }

function TTextItem.GetTranslated: boolean;
begin
  Result := (MsgId <> EmptyStr) and (MsgStr <> EmptyStr);
end;

{ TTextDomain }

function TTextDomain.AddText: TTextItem;
begin
end;

constructor TTextDomain.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTextItem);
end;

{ TPOParser }

procedure TPOParser.Clear;
begin
  FKeywords.Clear;
  FCommentLine := '#';
  FIdentifierCharset := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
  FSkipChar := '';
  FSkipEOL := True;
  FStringQuotes := '"';
  FSkipSpace := True;
end;

procedure TPOParser.ConstructCharset(const s: String);
var
  i: Integer;
begin
  FIdentifierCharset := [];
  for i := 1 to Length(s) do
    FIdentifierCharset := FIdentifierCharset + [s[i]];
end;

constructor TPOParser.Create;
begin
  FKeywords := TStringList.Create;
  TStringList(FKeywords).Sorted := True;
  FYList := TList.Create;
  FUseY := True;  
  Clear;
end;

destructor TPOParser.Destroy;
begin
  FKeywords.Free;
  FYList.Free;  
  inherited;
end;


function TPOParser.DoDigitSequence: Boolean;
begin
  Result := False;
  {$IFDEF UNICODE}
  if CharInSet(FText[FPosition], ['0'..'9', 'a'..'f', 'A'..'F']) then
  {$ELSE}
  if FText[FPosition] in ['0'..'9', 'a'..'f', 'A'..'F'] then
  {$ENDIF}
  begin
    {$IFDEF UNICODE}
    while CharInSet(FText[FPosition], ['0'..'9', 'a'..'f', 'A'..'F']) do
    {$ELSE}
    while FText[FPosition] in ['0'..'9', 'a'..'f', 'A'..'F'] do
    {$ENDIF}
      Inc(FPosition);
    Result := True;
  end;
end;

function TPOParser.DoUnsignedInteger: Boolean;
var
  Pos1: Integer;
begin
  Pos1 := FPosition;

  Result := DoDigitSequence;

  if not Result then
    FPosition := Pos1;
end;

function TPOParser.GetChar: String;
begin
  if FText[FPosition] in ['!', '@', '#', '$', '%', '^', '&', '|', '\',
    '.', ',', ':', ';', '?', '''', '"', '~', '`', '_', '[', ']', '{', '}',
    '(', ')', '+', '-', '*', '/', '=', '<', '>'] then
  begin
    Result := FText[FPosition];
    Inc(FPosition);
  end
  else
    Result := '';
end;

function TPOParser.GetEOL: Boolean;
begin
  SkipSpaces;
  if FText[FPosition] in [#10, #13] then
  begin
    Result := True;
    while FText[FPosition] in [#10, #13] do
      Inc(FPosition);
  end
  else
    Result := False;
end;

function TPOParser.GetIdent: String;
begin
  Result := Ident;
  if IsKeyWord(Result) then
    Result := '';
end;

function TPOParser.GetPlainPosition(pt: TPoint): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := pt.Y - 1;
  if (i >= 0) and (i < FYList.Count) then
    Result := Integer(FYList[i]) + pt.X;
end;

function TPOParser.GetString: String;
var
  Flag: Boolean;
  Str: String;
  FError: Boolean;
  FCpp: Boolean;

  function DoQuotedString: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    i := FPosition;

    if FText[FPosition] = FStringQuotes[1] then
    begin
      repeat
        Inc(FPosition);

        if FCpp and (FText[FPosition] = '\') then
        begin
          {$IFNDEF FPC}
          case Lowercase(FText[FPosition + 1])[1] of
          {$ELSE}
          case Lowercase(FText[FPosition + 1]) of
          {$ENDIF}
            'n':
              begin
                Str := Str + #10;
                Inc(FPosition);
              end;
            'r':
              begin
                Str := Str + #13;
                Inc(FPosition);
              end;
            else
              begin
                Str := Str + FText[FPosition + 1];
                Inc(FPosition);
              end;
          end;
        end
        else if FText[FPosition] = FStringQuotes[1] then
        begin
          if not FCpp and (FText[FPosition + 1] = FStringQuotes[1]) then
          begin
            Str := Str + FStringQuotes[1];
            Inc(FPosition);
          end
          else
            break
        end
        else
          Str := Str + FText[FPosition];
      until FText[FPosition] in [#0..#31] - [#9];

      if FText[FPosition] = FStringQuotes[1] then
      begin
        Inc(FPosition);
        Result := True;
      end
      else
        FPosition := i;
    end;
  end;

  function DoControlString: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    i := FPosition;

    if FText[FPosition] = '#' then
    begin
      Inc(FPosition);
      Result := DoUnsignedInteger;
      if Result then
        Str := Chr(StrToInt(Copy(FText, i + 1, FPosition - i - 1))) else
        FPosition := i;
    end;
  end;

begin
  Result := '';
  if FSkipSpace then
    SkipSpaces;
  Flag := True;
  FError := False;
  FCpp := FStringQuotes = '"';

  repeat
    Str := '';
    if DoQuotedString or DoControlString then
      Result := Result + Str
    else
    begin
      FError := Flag;
      break;
    end;

    Flag := False;
  until False;

  if not FError then
    Result := '''' + Result + '''';
end;

function TPOParser.GetWord: String;
begin
  Result := Ident;
end;

function TPOParser.GetXYPosition: String;
var
  i, i0, i1, c, pos, X, Y: Integer;
begin
  i0 := 0;
  i1 := FYList.Count - 1;

  while i0 <= i1 do
  begin
    i := (i0 + i1) div 2;
    pos := Integer(FYList[i]);

    if pos = FPosition then
      c := 0
    else if pos > FPosition then
      c := 1
    else
      c := -1;

    if c < 0 then
      i0 := i + 1
    else
    begin
      i1 := i - 1;
      if c = 0 then
        i0 := i;
    end;
  end;

  X := 1;
  Y := i0;
  i := Integer(FYList[i0 - 1]) + 1;

  while i < FPosition do
  begin
    Inc(i);
    Inc(X);
  end;

  Result := IntToStr(Y) + ':' + IntToStr(X);
end;

function TPOParser.Ident: String;
begin
  if FSkipSpace then
    SkipSpaces;

  if (FText[FPosition] in FIdentifierCharset - ['0'..'9']) then
  begin
    while FText[FPosition] in FIdentifierCharset do
      Inc(FPosition);
    Result := Copy(FText, FLastPosition, FPosition - FLastPosition);
  end
  else
    Result := '';
end;

function TPOParser.IsKeyWord(const s: String): Boolean;
begin
  Result := FKeywords.IndexOf(s) <> -1;
end;

procedure TPOParser.SetPosition(const Value: Integer);
begin
  FPosition := Value;
  FLastPosition := Value;
end;

procedure TPOParser.SetText(const Value: String);
var
  i: Integer;
begin
  FText := Value + #0;
  FLastPosition := 1;
  FPosition := 1;
  FSize := Length(Value);

  if FUseY then
  begin
    FYList.Clear;
    FYList.Add(TObject(0));
    for i := 1 to FSize do
      if FText[i] = #10 then
        FYList.Add(TObject(i));
  end;
end;

procedure TPOParser.SkipSpaces;
var
  s1: String;
  Spaces: set of Char;
begin
  Spaces := [#0..#32];
  if not FSkipEOL then
{$IFDEF LINUX}
    Spaces := Spaces - [#10];
{$ELSE}
    Spaces := Spaces - [#13];
{$ENDIF}
  while (FPosition <= FSize) and (FText[FPosition] in Spaces) do
    Inc(FPosition);
  { skip basic '_' }
  if (FPosition <= FSize) and (FSkipChar <> '') and (FText[FPosition] = FSkipChar[1]) then
  begin
    Inc(FPosition);
    GetEOL;
    SkipSpaces;
  end;

  if FPosition < FSize then
  begin
    if FCommentLine <> '' then
      s1 := Copy(FText, FPosition, Length(FCommentLine)) else
      s1 := ' ';
    if (s1 = FCommentLine) then
    begin
      while (FPosition <= FSize) and (FText[FPosition] <> #10) do
        Inc(FPosition);
      SkipSpaces;
    end;
  end;

  FLastPosition := FPosition;
end;

end.
