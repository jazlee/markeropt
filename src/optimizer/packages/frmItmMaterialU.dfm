object frmItmMaterial: TfrmItmMaterial
  Left = 0
  Top = 0
  Caption = 'Item Materials'
  ClientHeight = 444
  ClientWidth = 537
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
    Top = 424
    Width = 537
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
    ExplicitTop = 402
  end
  object cxTabControl1: TcxTabControl
    AlignWithMargins = True
    Left = 0
    Top = 51
    Width = 537
    Height = 373
    Margins.Left = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 351
    ClientRectBottom = 373
    ClientRectRight = 537
    ClientRectTop = 0
    object cxPageControl1: TcxPageControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 531
      Height = 367
      ActivePage = cxTabSheet1
      Align = alClient
      Focusable = False
      ShowFrame = True
      TabOrder = 0
      ExplicitHeight = 345
      ClientRectBottom = 366
      ClientRectLeft = 1
      ClientRectRight = 530
      ClientRectTop = 26
      object cxTabSheet1: TcxTabSheet
        Caption = 'Item Materials'
        ImageIndex = 0
        ExplicitHeight = 318
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
          Top = 70
          Width = 31
          Height = 13
          Caption = 'Name:'
          Transparent = True
        end
        object Label3: TLabel
          Left = 14
          Top = 94
          Width = 57
          Height = 13
          Caption = 'Description:'
          Transparent = True
        end
        object Label5: TLabel
          Left = 14
          Top = 47
          Width = 70
          Height = 13
          Caption = 'Material Code:'
          Transparent = True
        end
        object Label7: TLabel
          Left = 14
          Top = 142
          Width = 59
          Height = 13
          Caption = 'Laying Rule:'
          Enabled = False
          Transparent = True
          Visible = False
        end
        object Label9: TLabel
          Left = 14
          Top = 190
          Width = 47
          Height = 13
          Caption = 'UM Code:'
          Transparent = True
        end
        object Label10: TLabel
          Left = 14
          Top = 265
          Width = 67
          Height = 13
          Caption = 'FB Allowance:'
          Transparent = True
        end
        object Label8: TLabel
          Left = 14
          Top = 166
          Width = 42
          Height = 13
          Caption = 'Avg. YY:'
          Transparent = True
        end
        object Label4: TLabel
          Left = 214
          Top = 265
          Width = 37
          Height = 13
          Caption = 'Layout:'
          Transparent = True
        end
        object Label6: TLabel
          Left = 14
          Top = 118
          Width = 49
          Height = 13
          Caption = 'Category:'
          Transparent = True
        end
        object Label11: TLabel
          Left = 14
          Top = 289
          Width = 32
          Height = 13
          Caption = 'Width:'
          Transparent = True
        end
        object cxDBTextEdit1: TcxDBTextEdit
          Left = 87
          Top = 20
          DataBinding.DataField = 'itmname'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          TabOrder = 0
          Width = 121
        end
        object cxDBTextEdit2: TcxDBTextEdit
          Left = 87
          Top = 66
          DataBinding.DataField = 'mtname'
          DataBinding.DataSource = DataSource
          Enabled = False
          TabOrder = 2
          Width = 270
        end
        object cxDBTextEdit3: TcxDBTextEdit
          Left = 87
          Top = 90
          DataBinding.DataField = 'mtdesc'
          DataBinding.DataSource = DataSource
          Enabled = False
          TabOrder = 3
          Width = 348
        end
        object cxDBButtonEdit1: TcxDBButtonEdit
          Left = 87
          Top = 43
          DataBinding.DataField = 'mtcd'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit1PropertiesButtonClick
          TabOrder = 1
          Width = 121
        end
        object cxDBButtonEdit2: TcxDBButtonEdit
          Left = 87
          Top = 138
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
          TabOrder = 5
          Visible = False
          Width = 121
        end
        object cxDBTextEdit6: TcxDBTextEdit
          Left = 209
          Top = 138
          DataBinding.DataField = 'lrdesc'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          TabOrder = 6
          Visible = False
          Width = 226
        end
        object cxDBButtonEdit3: TcxDBButtonEdit
          Left = 87
          Top = 186
          DataBinding.DataField = 'umcode'
          DataBinding.DataSource = DataSource
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.CharCase = ecUpperCase
          Properties.OnButtonClick = cxDBButtonEdit3PropertiesButtonClick
          TabOrder = 8
          Width = 74
        end
        object cxDBRadioGroup1: TcxDBRadioGroup
          Left = 87
          Top = 213
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
          TabOrder = 9
          Height = 42
          Width = 306
        end
        object cxDBTextEdit8: TcxDBTextEdit
          Left = 87
          Top = 261
          DataBinding.DataField = 'fballowance'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 10
          Width = 121
        end
        object cxDBTextEdit7: TcxDBTextEdit
          Left = 87
          Top = 162
          DataBinding.DataField = 'avgyy'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 7
          Width = 121
        end
        object cxDBComboBox1: TcxDBComboBox
          Left = 287
          Top = 261
          DataBinding.DataField = 'mtlayout'
          DataBinding.DataSource = DataSource
          Properties.DropDownListStyle = lsEditFixedList
          Properties.DropDownSizeable = True
          Properties.Items.Strings = (
            'SINGLE'
            'TUBULAR')
          TabOrder = 11
          Width = 121
        end
        object cxDBTextEdit4: TcxDBTextEdit
          Left = 87
          Top = 114
          DataBinding.DataField = 'mtcatcd'
          DataBinding.DataSource = DataSource
          Enabled = False
          Properties.CharCase = ecUpperCase
          TabOrder = 4
          Width = 121
        end
        object cxDBTextEdit5: TcxDBTextEdit
          Left = 87
          Top = 285
          DataBinding.DataField = 'mtwidth'
          DataBinding.DataSource = DataSource
          Properties.CharCase = ecUpperCase
          TabOrder = 12
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
      Caption = 'Laying Rule Item'
      Enabled = False
      ImageIndex = 50
      Visible = False
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
