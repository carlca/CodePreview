{ This file is part of the CodePreview (for Lazarus/FPC) package

  Copyright (C) 1999-2017 - Carl Caulkett - carl.caulkett@gmail.com

  MODIFIED LGPL Licence - this is the same licence as that used by the Free Pascal Compiler (FPC)
  A copy of the full licence can be found in the file Licence.md in the same folder as this file.

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version
  with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with independent
  modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the
  resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module which is not derived from or based on this
  library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated
  to do so. If you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit preferencesunit;

{$mode objfpc}{$H+}
{$DEFINE XCODE}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, RTTIGrids, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Grids, ExtCtrls, ColorBox, Spin, casyncolorgrid, ComCtrls,
  cargbspinedit, cafontselector, caspeedbutton, LCLType, EditBtn, ComboEx,
  Buttons, casynconfig;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    Button1: TButton;
    caFontSelector1: TcaFontSelector;
    caSpeedButton1: TcaSpeedButton;
    Edit1: TEdit;
    Edit2: TEdit;
    FontDialog1: TFontDialog;
    OKButton: TButton;
    CancelButton: TButton;
    ColorGrid: TcaSynColorGrid;
    Pages: TPageControl;
    ColorsTab: TTabSheet;
    FontsTab: TTabSheet;
    TIPropertyGrid1: TTIPropertyGrid;
    TIPropertyGrid2: TTIPropertyGrid;
    UpDown1: TUpDown;
    procedure Button1Click(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    FModified: Boolean;
    FFontSelector: TcaFontSelector;
    function GetColorConfig: TcaSynConfig;
    procedure SetColorConfig(AValue: TcaSynConfig);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Modified: Boolean read FModified;
    property ColorConfig: TcaSynConfig read GetColorConfig write SetColorConfig;
  end;

var
  PreferencesForm: TPreferencesForm;

implementation

uses
  main;

{$R *.lfm}

{ TPreferencesForm }

constructor TPreferencesForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 567;
  Height := 390;
  Top := 70;
  Left := (Screen.Width div 2) - (Width div 2);
  OKButton.Left := (Width div 2) - OKButton.Width - 1;
  CancelButton.Left := OKButton.Left + OKButton.Width + 2;
end;

destructor TPreferencesForm.Destroy;
begin
  inherited Destroy;
end;

procedure TPreferencesForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TPreferencesForm.Button1Click(Sender: TObject);
begin
  FFontSelector := TcaFontSelector.Create(Self);
  FFontSelector.Parent := Self;
  FFontSelector.Left := 30;
  FFontSelector.Top := 30;
end;

procedure TPreferencesForm.OKButtonClick(Sender: TObject);
begin
  FModified := True;
  Close;
end;

function TPreferencesForm.GetColorConfig: TcaSynConfig;
begin
  Result := ColorGrid.ColorConfig;
end;

procedure TPreferencesForm.SetColorConfig(AValue: TcaSynConfig);
begin
  ColorGrid.ColorConfig := AValue;
end;

end.

