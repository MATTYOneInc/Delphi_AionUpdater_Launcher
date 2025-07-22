object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 432
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object updateIntervalLabel: TLabel
    Left = 8
    Top = 347
    Width = 140
    Height = 13
    Caption = #1048#1085#1090#1077#1088#1074#1072#1083' '#1087#1088#1086#1074#1077#1088#1082#1080' ('#1095#1072#1089#1099'):'
  end
  object playButton: TButton
    Left = 8
    Top = 293
    Width = 337
    Height = 25
    Caption = #1048#1075#1088#1072#1090#1100
    TabOrder = 0
    TabStop = False
    OnClick = playButtonClick
  end
  object checkButton: TButton
    Left = 8
    Top = 231
    Width = 193
    Height = 25
    Caption = 'checkButton'
    TabOrder = 1
    TabStop = False
    OnClick = checkButtonClick
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 337
    Height = 217
    TabOrder = 2
    object Memo1: TMemo
      Left = 8
      Top = 8
      Width = 321
      Height = 177
      TabStop = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 191
      Width = 321
      Height = 17
      TabOrder = 1
    end
  end
  object updateButton: TButton
    Left = 8
    Top = 262
    Width = 337
    Height = 25
    Caption = 'updateButton'
    TabOrder = 3
    TabStop = False
    OnClick = updateButtonClick
  end
  object stopButton: TButton
    Left = 207
    Top = 231
    Width = 138
    Height = 25
    Caption = 'stopButton'
    TabOrder = 4
    TabStop = False
    OnClick = stopButtonClick
  end
  object autoUpdateCheckBox: TCheckBox
    Left = 8
    Top = 324
    Width = 97
    Height = 17
    Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1087#1088#1086#1074#1077#1088#1103#1090#1100' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = autoUpdateCheckBoxClick
  end
  object updateIntervalEdit: TEdit
    Left = 168
    Top = 344
    Width = 121
    Height = 21
    TabStop = False
    TabOrder = 6
    Text = '24'
    OnChange = updateIntervalEditChange
  end
end
