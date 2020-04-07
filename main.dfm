object frmAltTabPro: TfrmAltTabPro
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'AltTabPro'
  ClientHeight = 240
  ClientWidth = 463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object UScrollBox1: TUScrollBox
    Left = 24
    Top = 24
    Width = 417
    Height = 208
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Color = 15132390
    ParentColor = False
    TabOrder = 0
    AniSet.AniKind = akOut
    AniSet.AniFunctionKind = afkQuintic
    AniSet.DelayStartTime = 0
    AniSet.Duration = 120
    AniSet.Step = 11
    CustomBackColor.Enabled = False
    CustomBackColor.Color = 15132390
    CustomBackColor.LightColor = 15132390
    CustomBackColor.DarkColor = 2039583
    object Memo1: TMemo
      Left = 24
      Top = 16
      Width = 312
      Height = 132
      Lines.Strings = (
        'Memo1')
      TabOrder = 1
    end
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 463
    Height = 240
    Align = alClient
    ItemHeight = 13
    Items.Strings = (
      'a'
      'b'
      'c'
      'd'
      'e'
      'f')
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 361
    ExplicitHeight = 169
  end
end
