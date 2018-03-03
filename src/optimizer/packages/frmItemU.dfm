object frmItem: TfrmItem
  Left = 0
  Top = 0
  Caption = 'Items'
  ClientHeight = 359
  ClientWidth = 496
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
    Top = 339
    Width = 496
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
    Width = 496
    Height = 288
    Margins.Left = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 0
    ClientRectBottom = 288
    ClientRectRight = 496
    ClientRectTop = 0
    object cxPageControl1: TcxPageControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 490
      Height = 282
      ActivePage = cxTabSheet1
      Align = alClient
      Focusable = False
      ShowFrame = True
      TabOrder = 0
      ClientRectBottom = 281
      ClientRectLeft = 1
      ClientRectRight = 489
      ClientRectTop = 26
      object cxTabSheet1: TcxTabSheet
        Caption = 'Items'
        ImageIndex = 0
        ExplicitLeft = 0
        ExplicitTop = 24
        object Label1: TLabel
          Left = 14
          Top = 24
          Width = 56
          Height = 13
          Caption = 'Item Name:'
          Transparent = True
        end
        object Label4: TLabel
          Left = 14
          Top = 49
          Width = 52
          Height = 13
          Caption = 'Item Desc:'
          Transparent = True
        end
        object Label5: TLabel
          Left = 14
          Top = 83
          Width = 28
          Height = 13
          Caption = 'Style:'
          Transparent = True
        end
        object Label2: TLabel
          Left = 14
          Top = 107
          Width = 51
          Height = 13
          Caption = 'X Feature:'
          Transparent = True
        end
        object Label6: TLabel
          Left = 232
          Top = 132
          Width = 22
          Height = 13
          Caption = 'Seq:'
          Transparent = True
        end
        object Label3: TLabel
          Left = 14
          Top = 130
          Width = 76
          Height = 13
          Caption = 'X Feature Item:'
          Transparent = True
        end
        object Label9: TLabel
          Left = 14
          Top = 161
          Width = 51
          Height = 13
          Caption = 'Y Feature:'
          Transparent = True
        end
        object Label11: TLabel
          Left = 14
          Top = 184
          Width = 76
          Height = 13
          Caption = 'Y Feature Item:'
          Transparent = True
        end
        object Label7: TLabel
          Left = 14
          Top = 215
          Width = 23
          Height = 13
          Caption = 'U/M:'
          Transparent = True
        end
        object Label8: TLabel
          Left = 350
          Top = 132
          Width = 22
          Height = 13
          Caption = 'Prio:'
          Transparent = True
        end
        object cxDBTextEdit1: TcxDBTextEdit
          Left = 105
          Top = 22
          DataBinding.DataField = 'itmname'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 0
          Width = 224
        end
        object cxDBTextEdit4: TcxDBTextEdit
          Left = 105
          Top = 45
          DataBinding.DataField = 'itmdesc'
          DataBinding.DataSource = DataSource
          TabOrder = 1
          Width = 348
        end
        object cxDBButtonEdit1: TcxDBButtonEdit
          Left = 105
          Top = 81
          DataBinding.DataField = 'stname'
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
          Left = 105
          Top = 105
          DataBinding.DataField = 'xftname'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit2PropertiesButtonClick
          TabOrder = 3
          Width = 121
        end
        object cxDBTextEdit2: TcxDBTextEdit
          Left = 260
          Top = 128
          DataBinding.DataField = 'xfitprio'
          DataBinding.DataSource = DataSource
          Properties.ReadOnly = False
          TabOrder = 5
          Width = 82
        end
        object cxDBButtonEdit4: TcxDBButtonEdit
          Left = 105
          Top = 128
          DataBinding.DataField = 'xfitname'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit4PropertiesButtonClick
          TabOrder = 4
          Width = 121
        end
        object cxDBButtonEdit5: TcxDBButtonEdit
          Left = 105
          Top = 159
          DataBinding.DataField = 'yftname'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit5PropertiesButtonClick
          TabOrder = 7
          Width = 121
        end
        object cxDBButtonEdit6: TcxDBButtonEdit
          Left = 105
          Top = 182
          DataBinding.DataField = 'yfitname'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit6PropertiesButtonClick
          TabOrder = 8
          Width = 121
        end
        object cxDBButtonEdit3: TcxDBButtonEdit
          Left = 105
          Top = 213
          DataBinding.DataField = 'umcode'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit3PropertiesButtonClick
          TabOrder = 9
          Width = 121
        end
        object cxDBTextEdit3: TcxDBTextEdit
          Left = 378
          Top = 128
          DataBinding.DataField = 'xfitprio2'
          DataBinding.DataSource = DataSource
          Properties.ReadOnly = False
          TabOrder = 6
          Width = 82
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
      Caption = 'Materials'
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
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'stdBtnReff_1'
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
