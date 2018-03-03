object frmProgress: TfrmProgress
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 101
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lbCaption: TLabel
    Left = 8
    Top = 14
    Width = 334
    Height = 13
    AutoSize = False
    Transparent = True
  end
  object DBProgress: TcxProgressBar
    Left = 8
    Top = 66
    Properties.BarStyle = cxbsAnimation
    Properties.BeginColor = 54056
    Properties.PeakValue = 60.000000000000000000
    TabOrder = 0
    Width = 334
  end
  object TBProgress: TcxProgressBar
    Left = 8
    Top = 36
    Properties.BarStyle = cxbsAnimation
    Properties.BeginColor = 54056
    Properties.PeakValue = 100.000000000000000000
    TabOrder = 1
    Width = 334
  end
end
