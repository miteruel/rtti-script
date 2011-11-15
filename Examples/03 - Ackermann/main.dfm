object frmAckermann: TfrmAckermann
  Left = 0
  Top = 0
  Caption = 'RTTI Script Ackermann Demo'
  ClientHeight = 328
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object btnRun: TButton
    Left = 280
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Run'
    Default = True
    TabOrder = 0
    OnClick = btnRunClick
  end
  object spnInput: TSpinEdit
    Left = 264
    Top = 176
    Width = 121
    Height = 26
    MaxValue = 5
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object StaticText1: TStaticText
    Left = 264
    Top = 150
    Width = 38
    Height = 20
    Caption = 'Input:'
    TabOrder = 2
  end
end
