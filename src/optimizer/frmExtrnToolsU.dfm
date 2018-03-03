object frmExtrnTools: TfrmExtrnTools
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'External Tools'
  ClientHeight = 331
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TcxListBox
    Left = 8
    Top = 22
    Width = 273
    Height = 301
    ItemHeight = 13
    Style.LookAndFeel.Kind = lfOffice11
    StyleDisabled.LookAndFeel.Kind = lfOffice11
    StyleFocused.LookAndFeel.Kind = lfOffice11
    StyleHot.LookAndFeel.Kind = lfOffice11
    TabOrder = 0
  end
  object cxLabel1: TcxLabel
    Left = 8
    Top = 4
    Caption = 'List of available tools:'
    Transparent = True
  end
  object btnAdd: TcxButton
    Left = 296
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnEdit: TcxButton
    Left = 296
    Top = 53
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 3
    OnClick = btnEditClick
  end
  object btnDelete: TcxButton
    Left = 296
    Top = 84
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
  object btnDown: TcxButton
    Left = 296
    Top = 178
    Width = 75
    Height = 25
    Caption = 'Move Down'
    TabOrder = 5
    OnClick = btnDownClick
  end
  object btnUp: TcxButton
    Left = 296
    Top = 147
    Width = 75
    Height = 25
    Caption = 'Move Up'
    TabOrder = 6
    OnClick = btnUpClick
  end
  object btnCLose: TcxButton
    Left = 296
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 7
  end
end
