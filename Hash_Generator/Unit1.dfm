object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 368
  ClientWidth = 448
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonScan: TButton
    Left = 8
    Top = 8
    Width = 433
    Height = 25
    Caption = 'ButtonScan'
    TabOrder = 0
    OnClick = ButtonScanClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 39
    Width = 433
    Height = 298
    Lines.Strings = (
      'MemoLog')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 343
    Width = 433
    Height = 17
    TabOrder = 2
  end
end
