object frmStyle: TfrmStyle
  Left = 0
  Top = 0
  Caption = 'Styles'
  ClientHeight = 387
  ClientWidth = 589
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
    Top = 368
    Width = 589
    Height = 19
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
    Width = 589
    Height = 317
    Margins.Left = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 0
    ClientRectBottom = 317
    ClientRectRight = 589
    ClientRectTop = 0
    object cxPageControl1: TcxPageControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 583
      Height = 311
      ActivePage = cxTabSheet1
      Align = alClient
      Focusable = False
      ShowFrame = True
      TabOrder = 0
      ClientRectBottom = 310
      ClientRectLeft = 1
      ClientRectRight = 582
      ClientRectTop = 26
      object cxTabSheet1: TcxTabSheet
        Caption = 'Styles'
        ImageIndex = 0
        object Label1: TLabel
          Left = 14
          Top = 24
          Width = 31
          Height = 13
          Caption = 'Name:'
          Transparent = True
        end
        object Label2: TLabel
          Left = 14
          Top = 48
          Width = 53
          Height = 13
          Caption = 'Description'
          Transparent = True
        end
        object Label3: TLabel
          Left = 14
          Top = 72
          Width = 55
          Height = 13
          Caption = 'Style Type:'
          Transparent = True
        end
        object Label4: TLabel
          Left = 14
          Top = 146
          Width = 51
          Height = 13
          Caption = 'U/M Code:'
          Transparent = True
        end
        object Label8: TLabel
          Left = 14
          Top = 121
          Width = 42
          Height = 13
          Caption = 'Avg. YY:'
          Transparent = True
        end
        object Label5: TLabel
          Left = 14
          Top = 97
          Width = 63
          Height = 13
          Caption = 'Item Format:'
          Transparent = True
        end
        object cxDBTextEdit1: TcxDBTextEdit
          Left = 77
          Top = 20
          DataBinding.DataField = 'stname'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 0
          Width = 121
        end
        object cxDBTextEdit2: TcxDBTextEdit
          Left = 77
          Top = 44
          DataBinding.DataField = 'stdesc'
          DataBinding.DataSource = DataSource
          TabOrder = 1
          Width = 270
        end
        object cxDBButtonEdit1: TcxDBButtonEdit
          Left = 77
          Top = 68
          DataBinding.DataField = 'sttname'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit1PropertiesButtonClick
          TabOrder = 2
          Width = 121
        end
        object cxDBButtonEdit2: TcxDBButtonEdit
          Left = 77
          Top = 142
          DataBinding.DataField = 'umcode'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit2PropertiesButtonClick
          TabOrder = 5
          Width = 72
        end
        object cxDBTextEdit7: TcxDBTextEdit
          Left = 77
          Top = 117
          DataBinding.DataField = 'avgyy'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 4
          Width = 121
        end
        object cxDBTextEdit3: TcxDBTextEdit
          Left = 77
          Top = 93
          DataBinding.DataField = 'itmfmt'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 3
          Width = 202
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
      Caption = 'Style Types'
      ImageIndex = 50
      OnExecute = actReff_1Execute
    end
    object actReff_2: TAction
      Caption = 'Features'
      ImageIndex = 50
      OnExecute = actReff_2Execute
    end
    object actReff_3: TAction
      Caption = 'Generate Items'
      ImageIndex = 12
      OnExecute = actReff_3Execute
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
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnReff_1'
        end
        item
          Visible = True
          ItemName = 'stdBtnReff_2'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnReff_3'
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
    object stdBtnReff_2: TdxBarLargeButton
      Action = actReff_2
      Category = 0
    end
    object stdBtnReff_3: TdxBarLargeButton
      Action = actReff_3
      Category = 0
    end
    object stdBtnPrint: TdxBarLargeButton
      Action = actPrint
      Category = 0
    end
  end
  object DataSource: TDataSource
    Left = 248
    Top = 74
  end
end
