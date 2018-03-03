unit ecfnavexceptions;

interface

uses
  SysUtils;

type
  EECFException = class(Exception);
  EECFNavException = class(EECFException);
  EECFMVCException = class(EECFNavException);
  EECFSecurityException = class(EECFNavException); 
  

implementation

end.
