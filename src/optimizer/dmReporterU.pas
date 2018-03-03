unit dmReporterU;

interface

uses
  SysUtils, Classes, frxClass, frxCrypt, frxDMPExport, frxGZip,
  frxExportImage, frxExportText, frxExportCSV, frxExportMail, frxExportODF,
  frxExportTXT, frxExportRTF, frxExportXML, frxExportXLS, frxExportHTML,
  frxExportPDF, frxOLE, frxRich, frxBarcode, frxDCtrl, frxCross;

type
  TdmReporter = class(TDataModule)
    frxReport: TfrxReport;
    frxPDFExport1: TfrxPDFExport;
    frxHTMLExport1: TfrxHTMLExport;
    frxXLSExport1: TfrxXLSExport;
    frxXMLExport1: TfrxXMLExport;
    frxRTFExport1: TfrxRTFExport;
    frxBMPExport1: TfrxBMPExport;
    frxTXTExport1: TfrxTXTExport;
    frxODTExport1: TfrxODTExport;
    frxODSExport1: TfrxODSExport;
    frxMailExport1: TfrxMailExport;
    frxCSVExport1: TfrxCSVExport;
    frxSimpleTextExport1: TfrxSimpleTextExport;
    frxGIFExport1: TfrxGIFExport;
    frxTIFFExport1: TfrxTIFFExport;
    frxJPEGExport1: TfrxJPEGExport;
    frxGZipCompressor1: TfrxGZipCompressor;
    frxDotMatrixExport1: TfrxDotMatrixExport;
    frxCrypt1: TfrxCrypt;
    frxUserDataSet1: TfrxUserDataSet;
    frxCrossObject1: TfrxCrossObject;
    frxDialogControls1: TfrxDialogControls;
    frxBarCodeObject1: TfrxBarCodeObject;
    frxRichObject1: TfrxRichObject;
    frxOLEObject1: TfrxOLEObject;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmReporter: TdmReporter;

implementation

{$R *.dfm}

end.
