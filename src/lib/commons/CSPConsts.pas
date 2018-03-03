unit CSPConsts;

interface
uses
  Windows, Messages;

const
  WM_APPMSG                         = WM_USER + 1;

  CMD_NONE                          = $00000000;
  CMD_PREPARE                       = $00000001;
  CMD_UNPREPARE                     = $00000002;
  CMD_UPDATESTATE                   = $00000003;
  CMD_CLOSE                         = $00000004;
  CMD_UPDATESTATS                   = $00000005;
  CMD_EXECCMD                       = $00000006;
  CMD_UNDO                          = $00000007;
  CMD_CUT                           = $00000008;
  CMD_COPY                          = $00000009;
  CMD_PASTE                         = $0000000A;
  CMD_SELECTALL                     = $0000000B;
  CMD_DELETE                        = $0000000C;
  CMD_PRINT                         = $0000000D;
  CMD_EXECPRINT                     = $0000000E;
  CMD_CLOSEACTIVEPROGRAM            = $0000000F;
  CMD_RESETFUNCS                    = $00000010;
  CMD_RECONSTRUCTMODS               = $00000012;
  CMD_REF_UOM                       = $00000050;
  CMD_REF_FEATURES                  = $00000051;
  CMD_REF_LAYINGRULES               = $00000052;
  CMD_REF_MATERIALS                 = $00000053;
  CMD_REF_CUSTOMERS                 = $00000054;
  CMD_REF_ITEMS                     = $00000055;
  CMD_REF_STYLES                    = $00000056;
  CMD_HLP_CONTENTS                  = $00000100;
  CMD_HLP_SEARCH                    = $00000101;
  CMD_HLP_HOW                       = $00000102;
  CMD_HLP_README                    = $00000103;
  CMD_HLP_CONTACT                   = $00000104;
  CMD_HLP_HOMEPAGE                  = $00000105;
  CMD_HLP_ABOUT                     = $00000106;
  CMD_PKG_NEW                       = $00001001;
  CMD_PKG_OPEN                      = $00001002;
  CMD_PKG_PROP                      = $00001003;
  CMD_PKG_REPAIR                    = $00001004;
  CMD_PKG_CLOSE                     = $00001009;
  CMD_ORD_NEW                       = $00003001;
  CMD_ORD_EDIT                      = $00003002;
  CMD_ORD_DELETE                    = $00003003;
  CMD_ORD_FIND                      = $00003004;
  CMD_ORD_OPTIMIZE                  = $00003005;
  CMD_ORD_PRINT                     = $00003006;
  CMD_USER                          = $00010000;

  RPT_UOM_ALL_ID                    = 1010;
  RPT_UOM_ITEM_ID                   = 1011;
  RPT_UOMCONV_ALL_ID                = 1020;
  RPT_UOMCONV_ITEM_ID               = 1021;
  RPT_FEATURE_ALL_ID                = 1030;
  RPT_FEATURE_ITEM_ID               = 1031;
  RPT_FITEM_ALL_ID                  = 1040;
  RPT_FITEM_ITEM_ID                 = 1041;
  RPT_LAYRULE_ALL_ID                = 1050;
  RPT_LAYRULE_ITEM_ID               = 1051;
  RPT_LRITEM_ALL_ID                 = 1060;
  RPT_LRITEM_ITEM_ID                = 1061;
  RPT_MATERIAL_ALL_ID               = 1070;
  RPT_MATERIAL_ITEM_ID              = 1071;
  RPT_CUSTOMER_ALL_ID               = 1080;
  RPT_CUSTOMER_ITEM_ID              = 1081;

  RPT_STYLE_ALL_ID                  = 2010;
  RPT_STYLE_ITEM_ID                 = 2011;
  RPT_STTYPE_ALL_ID                 = 2020;
  RPT_STTYPE_ITEM_ID                = 2021;
  RPT_STMATERIAL_ALL_ID             = 2030;
  RPT_STMATERIAL_ITEM_ID            = 2031;
  RPT_STFEATURE_ALL_ID              = 2040;
  RPT_STFEATURE_ITEM_ID             = 2041;
  RPT_STFTITEM_ALL_ID               = 2050;
  RPT_STFTITEM_ITEM_ID              = 2051;    
  RPT_STLRITM_ALL_ID                = 2060;
  RPT_STLRITM_ITEM_ID               = 2061;

  RPT_ITEM_ALL_ID                   = 3010;
  RPT_ITEM_ITEM_ID                  = 3011;
  RPT_ITMMATERIAL_ALL_ID            = 3020;
  RPT_ITMMATERIAL_ITEM_ID           = 3021;
  RPT_ITMLRITM_ALL_ID               = 3030;
  RPT_ITMLRITM_ITEM_ID              = 3031;

  RPT_ORDER_ID                      = 4001;
  RPT_ORDER_ITEM_ALL_ID             = 4010;
  RPT_ORDER_ITEM_ITEM_ID            = 4011;
  RPT_ORDER_MATERIAL_ALL_ID         = 4020;
  RPT_ORDER_MATERIAL_ITEM_ID        = 4021;
  RPT_ORDER_RULE_ALL_ID             = 4030;
  RPT_ORDER_RULE_ITEM_ID            = 4031;
  RPT_ORDER_STYLE_ALL_ID            = 4040;
  RPT_ORDER_STYLE_ITEM_ID           = 4041;

  DEF_NESTING_TIME                  = 1;
  
resourcestring
  SCSPInternalSettingFile           = 'settings.reg';
  SCSPDefaultSettingFile            = 'default.reg';
  SCSPExtrnToolsDataFile            = 'tools.reg';
  SCSPDefResDir                     = '\il18n';
  APP_READMEFILE                    = 'readme.rtf';

  DSNFileExt                        = 'dsn';
  DSPFileExt                        = 'dsp';
  BTFFileExt                        = 'btf';
  XMLFileExt                        = 'xml';
  NESTAppFileName                   = 'mark9.exe';

  BTFRemarkFormat                   = 'Optitex Batch File (*%s)|*%s';
  BTFSaveDialogTitle                = 'Save batch file as...';

  SCSPNewPkg                        = 'New package';
  SCSPOpenPkg                       = 'Open package';
  SCSPOpenCorruptedPkg              = 'Select corrupted package';
  SCSPCorruptedPkgFilter            = 'Packages (*.pkg)|*.pkg|References (*.db)|*.db|All Files (*.*)|*.*';
  SCSPPkgExt                        = '.pkg';
  SCSPPkgFilter                     = 'Packages (*.pkg)|*.pkg|All Files (*.*)|*.*';
  SCSPSelectApp                     = 'Select application';
  SCSPExeExt                        = '.exe';
  SCSPBatExt                        = '.bat';
  SCSPExeFilter                     = 'Program (*.exe)|*.exe|All Files (*.*)|*.*';

  SCSPRefDB                         = 'references.db';

  SCSPDefaultItemFormat             = 'SSSXXXYYY';
  SCSPNewPackage                    = 'This package has no description';
  SCSPWarnOverwriteLost             = 'Preparing this style will make any entered values will be lost, continue?';
  SCSPInvalidDBFile                 = 'The selected file was not a valid package file';

  SInfoBTFSaved                     = 'Batch file has been created and saved successfully';

  SConfirmDelCancel                 = 'Confirm to delete/cancel inserting this record?';
  SConfirmDelete                    = 'Confirm to delete this record?';
  SConfirmClose                     = 'Closing this window will make any changes to be canceled, continue?';
  SConfirmReplace                   = 'This will replace any existing value, continue?';
  SConfirmDeleteDSN                 = 'This will clear DSN data previously uploaded, continue?';
  SConfifmOptClear                  = 'This will clear the optimization result ever saved, continue?';

  SERRRefDataNotExist               = 'Reference data file does not exist, could not continue';
  SERRPkgDataNotExist               = 'Package data file has not been created, could not continue';
  SERRNotYFeature                   = 'Feature were not set in Y group';
  SERRSaveWorkFirst                 = 'Please save current edit before creating a new one';
  SERRUoMCircularConversion         = 'Circular reference between base and alternate U/M';
  SERRInvalidPassword               = 'Invalid password given';
  SERREntryHasNoReference           = 'No reference record found for those entry';
  SERREngineHasNotSetupProperly     = 'Engine has not been setup properly';
  SERROptitexUMCodeUndefined        = 'Unit of measurement for Optitex is undefined';
  SERRDSNFileNotSupplied            = 'DSN File has not been suplied';
  SERRMarkerDimensionNotDefined     = 'Marker dimension is not defined';
  SERRNestAppNotFound               = 'Nest application (%s) could not be found';
  SERRNoDeletionAllowed             = 'No deletion allowed on this module';
  SERRNoInsertionAllowed            = 'No insertion allowed on this module';
  // SERRAlgorithmNotDefined           = 'Algorithm used for nesting purpose is not defined';
implementation

end.

