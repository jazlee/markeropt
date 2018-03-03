{$define COMMERCIAL}

{$ifdef VER100}      // Delphi 3
 {$define D3}
 {$define DELPHI3}
 {$define DELPHI}
{$endif}

{$ifdef VER110}      // C++ Builder 3
 {$define D3}
 {$define DELPHI3}
 {$define BCB}
{$endif}

{$ifdef VER120}      // Delphi 4
 {$define D4}
 {$define DELPHI3}
 {$define DELPHI4}
 {$define DELPHI}
{$endif}

{$ifdef VER125}      // C++ Builder 4
 {$define D4}
 {$define DELPHI3}
 {$define DELPHI4}
 {$define BCB}
{$endif}

{$ifdef VER130}      // Delphi 5 or Builder 5
 {$define D5}
 {$define DELPHI3}
 {$define DELPHI4}
 {$define DELPHI5}
 {$ifndef BCB}        // Delphi 5
  {$define DELPHI}
 {$else}              // Builder 5
  {$undef DELPHI}	     // ( Ensure Delphi off )
 {$endif}
{$endif}

{$ifdef VER140}      // Delphi 6/Kylix.
 {$define D6}
 {$define DELPHI3}
 {$define DELPHI4}
 {$define DELPHI5}
 {$define DELPHI6}
 {$ifndef BCB}        // Delphi 6
  {$define DELPHI}
 {$else}              // Builder 6
  {$undef DELPHI}	     // ( Ensure Delphi off )
 {$endif}
{$endif}

{$ifdef VER150}      // Delphi 7.
 {$define D7}
 {$define DELPHI3}
 {$define DELPHI4}
 {$define DELPHI5}
 {$define DELPHI6}
 {$define DELPHI7}
 {$ifndef BCB}        // Delphi 7
    {$define DELPHI}
  {$else}              // Builder 7
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  {$ifdef LINUX}
    Sorry this DELPHI of Kylix not yet supported.
  {$endif}
{$endif}

{$ifdef VER170}      // Delphi 2005.
 {$define D9}
 {$define DELPHI3}
 {$define DELPHI4}
 {$define DELPHI5}
 {$define DELPHI6}
 {$define DELPHI7}
  {$define DELPHI9}
  {$ifndef BCB}        // Delphi 2005
    {$define DELPHI}
  {$else}
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  {$ifdef LINUX}
    Sorry this DELPHI of Kylix not yet supported.
  {$endif}
{$endif}

{$ifdef VER180}      // Delphi 2006.
 {$define D10}
 {$define DELPHI3}
 {$define DELPHI4}
 {$define DELPHI5}
 {$define DELPHI6}
 {$define DELPHI7}
 {$define DELPHI9}
 {$define DELPHI10}
  {$ifndef BCB}        // Delphi 2006
    {$define DELPHI}
  {$else}
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  {$ifdef LINUX}
    Sorry this DELPHI of Kylix not yet supported.
  {$endif}
{$endif}

{$ifdef VER185}      // Delphi 2007.
 {$define D11}
 {$define DELPHI3}
 {$define DELPHI4}
 {$define DELPHI5}
 {$define DELPHI6}
 {$define DELPHI7}
 {$define DELPHI9}
 {$define DELPHI10}
 {$define DELPHI11}
  {$ifndef BCB}        // Delphi 2007
    {$define DELPHI}
  {$else}
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  {$ifdef LINUX}
    Sorry this DELPHI of Kylix not yet supported.
  {$endif}
{$endif}

{$ifdef VER200}      // Delphi 2009.
 {$define D12}
 {$define DELPHI3}
 {$define DELPHI4}
 {$define DELPHI5}
 {$define DELPHI6}
 {$define DELPHI7}
 {$define DELPHI9}
 {$define DELPHI10}
 {$define DELPHI11}
 {$define DELPHI12}
  {$ifndef BCB}        // Delphi 2009
    {$define DELPHI}
  {$else}
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  {$ifdef LINUX}
    Sorry this DELPHI of Kylix not yet supported.
  {$endif}
{$endif}

{$ifdef CLR}
  {$define DOTNET}
  {$define DELPHI4}
  {$define DELPHI5}
  {$define DELPHI6}
  {$define DELPHI7}
  {$UNSAFECODE OFF}
{$ENDIF}

//***********************************************************************

{$ifdef BCB}
 {$ObjExportAll On}
 {$ASSERTIONS ON}
 {$define CPP}
{$endif}

{$ifdef DELPHI7}
 {$IFNDEF DOTNET}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
 {$ENDIF}
{$endif}

{$ifdef DELPHI6}
 {$WARN SYMBOL_PLATFORM OFF}
 {$WARN SYMBOL_DEPRECATED OFF}
 {$WARN UNIT_PLATFORM OFF}
 {$WARN UNIT_DEPRECATED OFF}
{$endif}

{$B-}    // Enable short circuit evaluation.