object frmCustomer: TfrmCustomer
  Left = 0
  Top = 0
  Caption = 'Customer'
  ClientHeight = 406
  ClientWidth = 579
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TdxStatusBar
    Left = 0
    Top = 386
    Width = 579
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        MinWidth = 250
        Width = 250
      end
      item
        PanelStyleClassName = 'TdxStatusBarKeyboardStatePanelStyle'
        PanelStyle.CapsLockKeyAppearance.ActiveCaption = 'CAPS'
        PanelStyle.CapsLockKeyAppearance.InactiveCaption = 'CAPS'
        PanelStyle.NumLockKeyAppearance.ActiveCaption = 'NUM'
        PanelStyle.NumLockKeyAppearance.InactiveCaption = 'NUM'
        PanelStyle.ScrollLockKeyAppearance.ActiveCaption = 'SCRL'
        PanelStyle.ScrollLockKeyAppearance.InactiveCaption = 'SCRL'
        PanelStyle.InsertKeyAppearance.ActiveCaption = 'OVR'
        PanelStyle.InsertKeyAppearance.InactiveCaption = 'INS'
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end>
    PaintStyle = stpsUseLookAndFeel
    LookAndFeel.Kind = lfOffice11
    LookAndFeel.NativeStyle = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object cxTabControl1: TcxTabControl
    AlignWithMargins = True
    Left = 0
    Top = 51
    Width = 579
    Height = 335
    Margins.Left = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 5
    ClientRectBottom = 335
    ClientRectRight = 579
    ClientRectTop = 0
    object cxPageControl1: TcxPageControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 573
      Height = 329
      ActivePage = cxTabSheet1
      Align = alClient
      Focusable = False
      ShowFrame = True
      TabOrder = 0
      ClientRectBottom = 328
      ClientRectLeft = 1
      ClientRectRight = 572
      ClientRectTop = 24
      object cxTabSheet1: TcxTabSheet
        Caption = 'Customers'
        ImageIndex = 0
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label1: TLabel
          Left = 14
          Top = 24
          Width = 29
          Height = 13
          Caption = 'Code:'
          Transparent = True
        end
        object Label2: TLabel
          Left = 14
          Top = 48
          Width = 31
          Height = 13
          Caption = 'Name:'
          Transparent = True
        end
        object Label3: TLabel
          Left = 14
          Top = 72
          Width = 57
          Height = 13
          Caption = 'Description:'
          Transparent = True
        end
        object Label4: TLabel
          Left = 14
          Top = 102
          Width = 43
          Height = 13
          Caption = 'Address:'
          Transparent = True
        end
        object Label7: TLabel
          Left = 14
          Top = 182
          Width = 43
          Height = 13
          Caption = 'Phone 1:'
          Transparent = True
        end
        object Label8: TLabel
          Left = 14
          Top = 206
          Width = 43
          Height = 13
          Caption = 'Phone 2:'
          Transparent = True
        end
        object cxDBTextEdit1: TcxDBTextEdit
          Left = 77
          Top = 20
          DataBinding.DataField = 'crdid'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 0
          Width = 121
        end
        object cxDBTextEdit2: TcxDBTextEdit
          Left = 77
          Top = 44
          DataBinding.DataField = 'crdname'
          DataBinding.DataSource = DataSource
          TabOrder = 1
          Width = 270
        end
        object cxDBTextEdit3: TcxDBTextEdit
          Left = 77
          Top = 68
          DataBinding.DataField = 'crddesc'
          DataBinding.DataSource = DataSource
          TabOrder = 2
          Width = 348
        end
        object cxDBTextEdit4: TcxDBTextEdit
          Left = 77
          Top = 98
          DataBinding.DataField = 'crdadr1'
          DataBinding.DataSource = DataSource
          TabOrder = 3
          Width = 270
        end
        object cxDBTextEdit5: TcxDBTextEdit
          Left = 77
          Top = 122
          DataBinding.DataField = 'crdadr2'
          DataBinding.DataSource = DataSource
          TabOrder = 4
          Width = 270
        end
        object cxDBTextEdit6: TcxDBTextEdit
          Left = 77
          Top = 146
          DataBinding.DataField = 'crdadr3'
          DataBinding.DataSource = DataSource
          TabOrder = 5
          Width = 270
        end
        object cxDBTextEdit7: TcxDBTextEdit
          Left = 77
          Top = 178
          DataBinding.DataField = 'crdphn1'
          DataBinding.DataSource = DataSource
          TabOrder = 6
          Width = 106
        end
        object cxDBTextEdit8: TcxDBTextEdit
          Left = 77
          Top = 202
          DataBinding.DataField = 'crdphn2'
          DataBinding.DataSource = DataSource
          TabOrder = 7
          Width = 106
        end
        object cxDBCheckBox1: TcxDBCheckBox
          Left = 77
          Top = 227
          Caption = 'Active'
          DataBinding.DataField = 'crdstat'
          DataBinding.DataSource = DataSource
          TabOrder = 8
          Transparent = True
          Width = 121
        end
      end
    end
  end
  object ActionList: TActionList
    Images = dmResources.LargeImages
    OnStateChange = ActionListStateChange
    Left = 220
    Top = 74
    object actNew: TAction
      Caption = '&New'
      ImageIndex = 0
      ShortCut = 16462
    end
    object actSave: TAction
      Caption = '&Save'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actDelete: TAction
      Caption = '&Delete'
      ImageIndex = 7
      OnExecute = actDeleteExecute
    end
    object actClose: TAction
      Caption = '&Close window'
      ImageIndex = 14
      ShortCut = 32856
      OnExecute = actCloseExecute
    end
    object actReff_1: TAction
      Caption = 'UoM Conversion'
      ImageIndex = 50
      OnExecute = actReff_1Execute
    end
    object actPrint: TAction
      Caption = 'Print'
      ImageIndex = 3
      OnExecute = actPrintExecute
    end
  end
  object BarManager: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    ImageOptions.Images = dmResources.SmallImages
    ImageOptions.LargeImages = dmResources.LargeImages
    PopupMenuLinks = <>
    Style = bmsUseLookAndFeel
    UseSystemFont = True
    Left = 192
    Top = 74
    DockControlHeights = (
      0
      0
      48
      0)
    object dxBarManager1Bar1: TdxBar
      Caption = 'Standard'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 326
      FloatTop = 216
      FloatClientWidth = 81
      FloatClientHeight = 181
      ItemLinks = <
        item
          Visible = True
          ItemName = 'stdBtnNew'
        end
        item
          Visible = True
          ItemName = 'stdBtnOpen'
        end
        item
          Visible = True
          ItemName = 'stdBtnPrint'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnDelete'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnClose'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      UseRestSpace = True
      Visible = True
      WholeRow = False
    end
    object stdBtnNew: TdxBarLargeButton
      Action = actNew
      Category = 0
    end
    object stdBtnOpen: TdxBarLargeButton
      Action = actSave
      Category = 0
    end
    object stdBtnPrint: TdxBarLargeButton
      Action = actPrint
      Category = 0
    end
    object stdBtnDelete: TdxBarLargeButton
      Action = actDelete
      Category = 0
    end
    object stdBtnClose: TdxBarLargeButton
      Action = actClose
      Category = 0
    end
    object stdBtnReff_1: TdxBarLargeButton
      Action = actReff_1
      Category = 0
    end
  end
  object DataSource: TDataSource
    Left = 248
    Top = 74
  end
end
