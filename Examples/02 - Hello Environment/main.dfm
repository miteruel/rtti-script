object frmHelloWorld: TfrmHelloWorld
  Left = 0
  Top = 0
  Caption = 'RTTI Script Hello Environment'
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
end
