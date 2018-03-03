object frmRegistration: TfrmRegistration
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Registration'
  ClientHeight = 297
  ClientWidth = 448
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object cxGroupBox1: TcxGroupBox
    Left = 8
    Top = 8
    Caption = ' Registration process '
    TabOrder = 0
    Height = 247
    Width = 430
    object Label1: TLabel
      Left = 13
      Top = 24
      Width = 394
      Height = 211
      Caption = 
        'There are three steps you should perform to be sucessfully regis' +
        'ter this software.'#13#10#13#10'1. Send the validation key shown in the th' +
        'e edit box below via e-mail.'#13#10#13#10#13#10#13#10#13#10'2. We will send you back t' +
        'he appropriate confirmation key for the validation key '#13#10'    you' +
        ' have sent (usualy as an attachment named license.txt). Open the' +
        ' file '#13#10'    using notepad or any text editor, copy and paste the' +
        ' confirmation key into'#13#10'    the edit box below.'#13#10#13#10#13#10#13#10#13#10'3. You ' +
        'will need to restart the application to make the license fully a' +
        'pplied.'
      Transparent = True
    end
    object Label2: TLabel
      Left = 26
      Top = 70
      Width = 70
      Height = 13
      Caption = 'Validation key:'
      Transparent = True
    end
    object Label3: TLabel
      Left = 26
      Top = 174
      Width = 85
      Height = 13
      Caption = 'Confirmation key:'
      Transparent = True
    end
    object edMachineID: TcxTextEdit
      Left = 26
      Top = 86
      Properties.ReadOnly = True
      Style.Color = clBtnFace
      TabOrder = 0
      Width = 381
    end
    object edRegEdit: TcxTextEdit
      Left = 26
      Top = 190
      TabOrder = 1
      Width = 381
    end
  end
  object btnRegister: TButton
    Left = 269
    Top = 261
    Width = 75
    Height = 25
    Caption = 'Register'
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 350
    Top = 261
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
