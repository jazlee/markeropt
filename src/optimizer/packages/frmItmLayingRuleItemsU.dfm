object frmItmLayingRuleItems: TfrmItmLayingRuleItems
  Left = 0
  Top = 0
  Caption = 'Item Rule Items'
  ClientHeight = 336
  ClientWidth = 480
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
    Top = 316
    Width = 480
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
    Width = 480
    Height = 265
    Margins.Left = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 0
    ClientRectBottom = 265
    ClientRectRight = 480
    ClientRectTop = 0
    object cxPageControl1: TcxPageControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 474
      Height = 259
      ActivePage = cxTabSheet1
      Align = alClient
      Focusable = False
      ShowFrame = True
      TabOrder = 0
      ClientRectBottom = 258
      ClientRectLeft = 1
      ClientRectRight = 473
      ClientRectTop = 26
      object cxTabSheet1: TcxTabSheet
        Caption = 'Item Rule Items'
        ImageIndex = 0
        object Label1: TLabel
          Left = 14
          Top = 24
          Width = 26
          Height = 13
          Caption = 'Item:'
          Transparent = True
        end
        object Label2: TLabel
          Left = 14
          Top = 76
          Width = 54
          Height = 13
          Caption = 'Min. Layer:'
          Transparent = True
        end
        object Label5: TLabel
          Left = 14
          Top = 51
          Width = 42
          Height = 13
          Caption = 'Material:'
          Transparent = True
        end
        object cxDBTextEdit1: TcxDBTextEdit
          Left = 89
          Top = 20
          DataBinding.DataField = 'itmname'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          Properties.ReadOnly = True
          TabOrder = 0
          Width = 121
        end
        object cxDBSpinEdit1: TcxDBSpinEdit
          Left = 89
          Top = 72
          DataBinding.DataField = 'minlyr'
          DataBinding.DataSource = DataSource
          TabOrder = 2
          Width = 121
        end
        object cxDBTextEdit4: TcxDBTextEdit
          Left = 89
          Top = 47
          DataBinding.DataField = 'mtcd'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          Properties.ReadOnly = True
          TabOrder = 1
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
    object actPrint: TAction
      Caption = 'Print'
      ImageIndex = 3
      OnExecute = actPrintExecute
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
