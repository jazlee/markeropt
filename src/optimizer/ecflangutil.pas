unit ecflangutil;

interface

uses
  SysUtils, Classes;

procedure ReconstructLanguage(AComponent: TComponent; const domain: String = '');

implementation
uses
  TypInfo, gnugettext;

procedure ReconstructLanguage(AComponent: TComponent; const domain: String);
var
  I: Integer;
  PropInfo: PPropInfo;
  AStr: String;
begin
  PropInfo := GetPropInfo(AComponent.ClassType, 'Caption', tkProperties);
  if PropInfo <> nil then
  begin
    AStr := GetStrProp(AComponent, 'Caption');
    if domain = EmptyStr then    
      SetStrProp(AComponent, 'Caption', _(AStr))
    else
      SetStrProp(AComponent, 'Caption', dgettext(domain, AStr));
  end;
  for I := 0 to AComponent.ComponentCount - 1 do
    ReconstructLanguage(AComponent.Components[I]);
end;

end.
