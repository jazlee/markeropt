object frmStMaterial: TfrmStMaterial
  Left = 0
  Top = 0
  Caption = 'Style Materials'
  ClientHeight = 460
  ClientWidth = 486
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
    Top = 440
    Width = 486
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
    ExplicitTop = 423
  end
  object cxTabControl1: TcxTabControl
    AlignWithMargins = True
    Left = 0
    Top = 51
    Width = 486
    Height = 389
    Margins.Left = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 372
    ClientRectBottom = 389
    ClientRectRight = 486
    ClientRectTop = 0
    object cxPageControl1: TcxPageControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 480
      Height = 383
      ActivePage = cxTabSheet1
      Align = alClient
      Focusable = False
      ShowFrame = True
      TabOrder = 0
      ExplicitHeight = 366
      ClientRectBottom = 382
      ClientRectLeft = 1
      ClientRectRight = 479
      ClientRectTop = 26
      object cxTabSheet1: TcxTabSheet
        Caption = 'Style Materials'
        ImageIndex = 0
        ExplicitHeight = 339
        object Label1: TLabel
          Left = 14
          Top = 24
          Width = 28
          Height = 13
          Caption = 'Style:'
          Transparent = True
        end
        object Label2: TLabel
          Left = 14
          Top = 120
          Width = 31
          Height = 13
          Caption = 'Name:'
          Transparent = True
        end
        object Label3: TLabel
          Left = 14
          Top = 144
          Width = 57
          Height = 13
          Caption = 'Description:'
          Transparent = True
        end
        object Label4: TLabel
          Left = 14
          Top = 48
          Width = 42
          Height = 13
          Caption = 'Feature:'
          Transparent = True
        end
        object Label5: TLabel
          Left = 14
          Top = 71
          Width = 26
          Height = 13
          Caption = 'Item:'
          Transparent = True
        end
        object Label6: TLabel
          Left = 14
          Top = 96
          Width = 42
          Height = 13
          Caption = 'Material:'
          Transparent = True
        end
        object Label7: TLabel
          Left = 14
          Top = 190
          Width = 59
          Height = 13
          Caption = 'Laying Rule:'
          Enabled = False
          Transparent = True
          Visible = False
        end
        object Label9: TLabel
          Left = 14
          Top = 214
          Width = 47
          Height = 13
          Caption = 'UM Code:'
          Transparent = True
        end
        object Label10: TLabel
          Left = 14
          Top = 289
          Width = 67
          Height = 13
          Caption = 'FB Allowance:'
          Transparent = True
        end
        object Label8: TLabel
          Left = 241
          Top = 288
          Width = 37
          Height = 13
          Caption = 'Layout:'
          Transparent = True
        end
        object Label11: TLabel
          Left = 14
          Top = 167
          Width = 49
          Height = 13
          Caption = 'Category:'
          Transparent = True
        end
        object Label12: TLabel
          Left = 14
          Top = 313
          Width = 32
          Height = 13
          Caption = 'Width:'
          Transparent = True
        end
        object cxDBTextEdit1: TcxDBTextEdit
          Left = 87
          Top = 20
          DataBinding.DataField = 'stname'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          TabOrder = 0
          Width = 121
        end
        object cxDBTextEdit2: TcxDBTextEdit
          Left = 87
          Top = 116
          DataBinding.DataField = 'mtname'
          DataBinding.DataSource = DataSource
          Enabled = False
          TabOrder = 4
          Width = 270
        end
        object cxDBTextEdit3: TcxDBTextEdit
          Left = 87
          Top = 140
          DataBinding.DataField = 'mtdesc'
          DataBinding.DataSource = DataSource
          Enabled = False
          TabOrder = 5
          Width = 348
        end
        object cxDBTextEdit4: TcxDBTextEdit
          Left = 87
          Top = 44
          DataBinding.DataField = 'ftname'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          TabOrder = 1
          Width = 121
        end
        object cxDBTextEdit5: TcxDBTextEdit
          Left = 87
          Top = 67
          DataBinding.DataField = 'fitname'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          TabOrder = 2
          Width = 121
        end
        object cxDBButtonEdit1: TcxDBButtonEdit
          Left = 87
          Top = 92
          DataBinding.DataField = 'mtcd'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit1PropertiesButtonClick
          TabOrder = 3
          Width = 121
        end
        object cxDBButtonEdit2: TcxDBButtonEdit
          Left = 87
          Top = 187
          DataBinding.DataField = 'lrname'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit2PropertiesButtonClick
          TabOrder = 7
          Visible = False
          Width = 121
        end
        object cxDBTextEdit6: TcxDBTextEdit
          Left = 210
          Top = 187
          DataBinding.DataField = 'lrdesc'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          TabOrder = 8
          Visible = False
          Width = 225
        end
        object cxDBButtonEdit3: TcxDBButtonEdit
          Left = 87
          Top = 210
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
          Width = 74
        end
        object cxDBRadioGroup1: TcxDBRadioGroup
          Left = 87
          Top = 237
          Caption = 'Allowance Type '
          DataBinding.DataField = 'fbaltype'
          DataBinding.DataSource = DataSource
          Properties.Columns = 2
          Properties.Items = <
            item
              Caption = 'Percentage'
              Value = '0'
            end
            item
              Caption = 'Value'
              Value = '1'
            end>
          TabOrder = 10
          Height = 42
          Width = 348
        end
        object cxDBTextEdit8: TcxDBTextEdit
          Left = 87
          Top = 285
          DataBinding.DataField = 'fballowance'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 11
          Width = 121
        end
        object cxDBComboBox1: TcxDBComboBox
          Left = 314
          Top = 285
          DataBinding.DataField = 'mtlayout'
          DataBinding.DataSource = DataSource
          Properties.DropDownListStyle = lsEditFixedList
          Properties.DropDownSizeable = True
          Properties.Items.Strings = (
            'SINGLE'
            'TUBULAR')
          TabOrder = 12
          Width = 121
        end
        object cxDBTextEdit7: TcxDBTextEdit
          Left = 87
          Top = 163
          DataBinding.DataField = 'mtcatcd'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          TabOrder = 6
          Width = 121
        end
        object cxDBTextEdit9: TcxDBTextEdit
          Left = 87
          Top = 309
          DataBinding.DataField = 'mtwidth'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 13
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
      Caption = 'Laying Rule Details'
      Enabled = False
      ImageIndex = 50
      Visible = False
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
