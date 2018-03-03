object frmOptions: TfrmOptions
  Left = 513
  Top = 182
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 396
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object cxPageControl1: TcxPageControl
    Left = 12
    Top = 8
    Width = 375
    Height = 345
    ActivePage = GeneralSheet
    Focusable = False
    LookAndFeel.Kind = lfOffice11
    LookAndFeel.NativeStyle = False
    MultiLine = True
    ShowFrame = True
    TabOrder = 0
    ClientRectBottom = 344
    ClientRectLeft = 1
    ClientRectRight = 374
    ClientRectTop = 26
    object GeneralSheet: TcxTabSheet
      Caption = 'General'
      ImageIndex = 1
      object Bevel1: TBevel
        Left = 90
        Top = 23
        Width = 267
        Height = 3
        Shape = bsTopLine
      end
      object Bevel5: TBevel
        Left = 90
        Top = 147
        Width = 267
        Height = 3
        Shape = bsTopLine
      end
      object Bevel6: TBevel
        Left = 56
        Top = 216
        Width = 301
        Height = 3
        Shape = bsTopLine
      end
      object cxLabel2: TcxLabel
        Left = 6
        Top = 14
        Caption = 'File extensions'
        Transparent = True
      end
      object cxLabel3: TcxLabel
        Left = 14
        Top = 40
        Caption = 'PDS Extension:'
        Transparent = True
      end
      object edDSN: TcxTextEdit
        Left = 170
        Top = 40
        TabOrder = 2
        Width = 67
      end
      object cxLabel4: TcxLabel
        Left = 14
        Top = 63
        Caption = 'Marker Extension:'
        Transparent = True
      end
      object edDSP: TcxTextEdit
        Left = 170
        Top = 64
        TabOrder = 4
        Width = 67
      end
      object cxLabel9: TcxLabel
        Left = 14
        Top = 85
        Caption = 'BTF Extension:'
        Transparent = True
      end
      object edBTF: TcxTextEdit
        Left = 170
        Top = 86
        TabOrder = 6
        Width = 67
      end
      object cxLabel10: TcxLabel
        Left = 14
        Top = 107
        Caption = 'XML Extension:'
        Transparent = True
      end
      object edXML: TcxTextEdit
        Left = 170
        Top = 108
        TabOrder = 8
        Width = 67
      end
      object cxLabel11: TcxLabel
        Left = 6
        Top = 138
        Caption = 'Nest application'
        Transparent = True
      end
      object cxLabel12: TcxLabel
        Left = 14
        Top = 161
        Caption = 'Nest file application:'
        Transparent = True
      end
      object edNestApp: TcxButtonEdit
        Left = 14
        Top = 180
        Properties.Buttons = <
          item
            Default = True
            Kind = bkEllipsis
          end>
        Properties.ReadOnly = True
        Properties.OnButtonClick = edNestAppPropertiesButtonClick
        TabOrder = 11
        Width = 343
      end
      object cxLabel13: TcxLabel
        Left = 6
        Top = 207
        Caption = 'Nesting'
        Transparent = True
      end
      object cxLabel14: TcxLabel
        Left = 14
        Top = 230
        Caption = 'Default time per step:'
        Transparent = True
      end
      object edNestTime: TcxSpinEdit
        Left = 170
        Top = 227
        TabOrder = 14
        Width = 67
      end
      object cxLabel15: TcxLabel
        Left = 243
        Top = 229
        Caption = 'minute(s)'
        Transparent = True
      end
    end
    object PreferenceSheet: TcxTabSheet
      Caption = 'Preferences'
      ImageIndex = 0
      object Bevel2: TBevel
        Left = 60
        Top = 23
        Width = 297
        Height = 3
        Shape = bsTopLine
      end
      object Bevel3: TBevel
        Left = 51
        Top = 109
        Width = 304
        Height = 3
        Shape = bsTopLine
      end
      object Bevel4: TBevel
        Left = 104
        Top = 199
        Width = 253
        Height = 3
        Shape = bsTopLine
      end
      object ckSplash: TcxCheckBox
        Left = 12
        Top = 37
        Caption = 'Show splash screen at startup'
        Style.HotTrack = False
        Style.LookAndFeel.Kind = lfOffice11
        Style.LookAndFeel.NativeStyle = False
        StyleDisabled.LookAndFeel.Kind = lfOffice11
        StyleDisabled.LookAndFeel.NativeStyle = False
        StyleFocused.LookAndFeel.Kind = lfOffice11
        StyleFocused.LookAndFeel.NativeStyle = False
        StyleHot.LookAndFeel.Kind = lfOffice11
        StyleHot.LookAndFeel.NativeStyle = False
        TabOrder = 0
        Transparent = True
        Width = 180
      end
      object cxLabel1: TcxLabel
        Left = 6
        Top = 14
        Caption = 'Options'
        Transparent = True
      end
      object ckMultiInstance: TcxCheckBox
        Left = 12
        Top = 53
        Caption = 'Enable multiple instances'
        Style.HotTrack = False
        Style.LookAndFeel.Kind = lfOffice11
        Style.LookAndFeel.NativeStyle = False
        StyleDisabled.LookAndFeel.Kind = lfOffice11
        StyleDisabled.LookAndFeel.NativeStyle = False
        StyleFocused.LookAndFeel.Kind = lfOffice11
        StyleFocused.LookAndFeel.NativeStyle = False
        StyleHot.LookAndFeel.Kind = lfOffice11
        StyleHot.LookAndFeel.NativeStyle = False
        TabOrder = 2
        Transparent = True
        Width = 180
      end
      object cxLabel5: TcxLabel
        Left = 6
        Top = 100
        Caption = 'Themes'
        Transparent = True
      end
      object cxLabel6: TcxLabel
        Left = 12
        Top = 122
        Caption = 'Setup the default theme to be used'
        Transparent = True
      end
      object cxLabel7: TcxLabel
        Left = 6
        Top = 190
        Caption = 'Internationalisation'
        Transparent = True
      end
      object cxLabel8: TcxLabel
        Left = 12
        Top = 211
        Caption = 'Setup default language for this application'
        Transparent = True
      end
      object cbTheme: TcxComboBox
        Left = 60
        Top = 141
        TabOrder = 7
        Width = 159
      end
      object cbLang: TcxComboBox
        Left = 60
        Top = 234
        TabOrder = 8
        Width = 159
      end
      object ckRibbon: TcxCheckBox
        Left = 60
        Top = 160
        Caption = 'Use ribbon bar'
        Style.HotTrack = False
        Style.LookAndFeel.Kind = lfOffice11
        Style.LookAndFeel.NativeStyle = False
        StyleDisabled.LookAndFeel.Kind = lfOffice11
        StyleDisabled.LookAndFeel.NativeStyle = False
        StyleFocused.LookAndFeel.Kind = lfOffice11
        StyleFocused.LookAndFeel.NativeStyle = False
        StyleHot.LookAndFeel.Kind = lfOffice11
        StyleHot.LookAndFeel.NativeStyle = False
        TabOrder = 9
        Transparent = True
        Width = 143
      end
      object btnRegister: TcxButton
        Left = 12
        Top = 283
        Width = 127
        Height = 25
        Caption = 'Register this application'
        TabOrder = 10
        OnClick = btnRegisterClick
      end
    end
  end
  object btnOK: TcxButton
    Left = 233
    Top = 363
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TcxButton
    Left = 311
    Top = 363
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
