object xfrFileViewerForm: TxfrFileViewerForm
  Left = 433
  Top = 200
  Width = 503
  Height = 372
  Caption = 'File Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 495
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      495
      41)
    object Label1: TLabel
      Left = 6
      Top = 14
      Width = 19
      Height = 13
      Caption = 'File:'
    end
    object btnCancel: TButton
      Left = 412
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object Button2: TButton
      Left = 332
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object Edit1: TEdit
      Left = 32
      Top = 10
      Width = 289
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      Text = 'Edit1'
    end
  end
  object CodePanel: TPanel
    Left = 0
    Top = 41
    Width = 495
    Height = 285
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 2
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 326
    Width = 495
    Height = 19
    Panels = <>
  end
end
