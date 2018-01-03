object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Minesweeper'
  ClientHeight = 58
  ClientWidth = 275
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object StatusLabel: TLabel
    Left = 147
    Top = 29
    Width = 70
    Height = 13
  end
  object Label1: TLabel
    Left = 24
    Top = 5
    Width = 152
    Height = 13
    Caption = 'Brady Coles - CMPT 360 Lab #5'
  end
  object GridButton1: TGridButton
    Left = 24
    Top = 24
    Width = 91
    Height = 25
    Caption = 'Start New Game'
    TabOrder = 0
    OnClick = NewGameClick
    X = 0
    Y = 0
  end
end
