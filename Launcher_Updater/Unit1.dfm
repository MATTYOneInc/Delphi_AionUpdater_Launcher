object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 567
  ClientWidth = 889
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
  object Panel1: TPanel
    Left = 447
    Top = 8
    Width = 433
    Height = 551
    TabOrder = 0
    object Memo1: TMemo
      Left = 8
      Top = 8
      Width = 417
      Height = 537
      TabStop = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 8
    Width = 433
    Height = 357
    TabOrder = 1
    object WebBrowser1: TWebBrowser
      Left = 1
      Top = 1
      Width = 431
      Height = 355
      TabStop = False
      Align = alClient
      TabOrder = 0
      SelectedEngine = EdgeIfAvailable
      ControlData = {
        4C0000008C2C0000B12400000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126208000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
  object Panel3: TPanel
    Left = 8
    Top = 371
    Width = 433
    Height = 54
    TabOrder = 2
    object statusLabel: TLabel
      Left = 8
      Top = 31
      Width = 110
      Height = 13
      Caption = #1057#1090#1072#1090#1091#1089': '#1053#1077#1080#1079#1074#1077#1089#1090#1085#1099#1081
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 8
      Width = 417
      Height = 17
      TabOrder = 0
    end
  end
  object Panel4: TPanel
    Left = 8
    Top = 431
    Width = 433
    Height = 130
    TabOrder = 3
    object updateIntervalLabel: TLabel
      Left = 238
      Top = 73
      Width = 140
      Height = 13
      Caption = #1048#1085#1090#1077#1088#1074#1072#1083' '#1087#1088#1086#1074#1077#1088#1082#1080' ('#1095#1072#1089#1099'):'
    end
    object checkButton: TButton
      Left = 8
      Top = 9
      Width = 210
      Height = 25
      Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100' '#1082#1083#1080#1077#1085#1090
      TabOrder = 0
      TabStop = False
      OnClick = checkButtonClick
    end
    object stopButton: TButton
      Left = 224
      Top = 8
      Width = 201
      Height = 25
      Caption = #1054#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1087#1088#1086#1074#1077#1088#1082#1091
      TabOrder = 1
      TabStop = False
      OnClick = stopButtonClick
    end
    object updateButton: TButton
      Left = 8
      Top = 39
      Width = 417
      Height = 25
      Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103
      TabOrder = 2
      TabStop = False
      OnClick = updateButtonClick
    end
    object playButton: TButton
      Left = 8
      Top = 97
      Width = 417
      Height = 25
      Caption = #1048#1075#1088#1072#1090#1100
      Enabled = False
      TabOrder = 3
      TabStop = False
      OnClick = playButtonClick
    end
    object autoUpdateCheckBox: TCheckBox
      Left = 8
      Top = 70
      Width = 225
      Height = 17
      TabStop = False
      Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1087#1088#1086#1074#1077#1088#1103#1090#1100' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = autoUpdateCheckBoxClick
    end
    object updateIntervalEdit: TEdit
      Left = 384
      Top = 70
      Width = 41
      Height = 21
      TabStop = False
      Alignment = taCenter
      TabOrder = 5
      Text = '24'
      OnChange = updateIntervalEditChange
    end
  end
end
