{ This file is part of the CodePreview (for Lazarus/FPC) package

  Copyright (C) 1999-2017 - Carl Caulkett - carl.caulkett@gmail.com

  MODIFIED LGPL Licence - this is the same licence as that used by the Free Pascal Compiler (FPC)
  A copy of the full licence can be found in the file Licence.md in the same folder as this file.

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Li7brary General Public
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

  // TPreferencesForm

  TPreferencesForm = class(TForm)
    // form components
    FilesFontSelector: TcaFontSelector;
    FoldersFontSelector: TcaFontSelector;
    GolangFontSelector: TcaFontSelector;
    ColorGrid: TcaSynColorGrid;
    Pages: TPageControl;
    ColorsTab: TTabSheet;
    FontsTab: TTabSheet;
    // form component event handlers
    procedure FilesFontSelectorSelectedFontChanged(Sender: TObject; AFont: TFont);
    procedure FoldersFontSelectorSelectedFontChanged(Sender: TObject; AFont: TFont);
    procedure GolangFontSelectorSelectedFontChanged(Sender: TObject; AFont: TFont);
  private
    // private declarations
    FFontSelector: TcaFontSelector;
    FOnFilesFontChanged: TcaFontChangedEvent;
    FOnFoldersFontChanged: TcaFontChangedEvent;
    FOnGolangFontChanged: TcaFontChangedEvent;
    FOnColorGridChanged: TNotifyEvent;
    // private event handlers
    procedure ColorGridChangedEvent(Sender: TObject);
    // private property methods
    function GetColorConfig: TcaSynConfig;
    procedure SetColorConfig(AValue: TcaSynConfig);
  protected
    // protected methods
    procedure DoFilesFontChanged(AFont: TFont); virtual;
    procedure DoFoldersFontChanged(AFont: TFont); virtual;
    procedure DoGolangFontChanged(AFont: TFont); virtual;
    procedure DoColorFieldChanged; virtual;
  public
    // create/destroy
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // public methods
    procedure SetFonts(FilesFont, FoldersFont, GolangFont: TFont);
    // public properties
    property ColorConfig: TcaSynConfig read GetColorConfig write SetColorConfig;
    property OnFilesFontChanged: TcaFontChangedEvent read FOnFilesFontChanged write FOnFilesFontChanged;
    property OnFoldersFontChanged: TcaFontChangedEvent read FOnFoldersFontChanged write FOnFoldersFontChanged;
    property OnGolangFontChanged: TcaFontChangedEvent read FOnGolangFontChanged write FOnGolangFontChanged;
    property OnColorGridChanged: TNotifyEvent read FOnColorGridChanged write FOnColorGridChanged;
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
  Height := 360;
  Top := 70;
  Left := (Screen.Width div 2) - (Width div 2);
  Cursor := crDefault;
  ColorGrid.OnColorGridChanged := @ColorGridChangedEvent;
end;

destructor TPreferencesForm.Destroy;
begin
  inherited Destroy;
end;

procedure TPreferencesForm.SetFonts(FilesFont, FoldersFont, GolangFont: TFont);
begin
  FilesFontSelector.SelectedFont := FilesFont;
  FoldersFontSelector.SelectedFont := FoldersFont;
  GolangFontSelector.SelectedFont := GolangFont;
end;

procedure TPreferencesForm.FilesFontSelectorSelectedFontChanged(Sender: TObject; AFont: TFont);
begin
  DoFilesFontChanged(AFont);
end;

procedure TPreferencesForm.FoldersFontSelectorSelectedFontChanged(Sender: TObject; AFont: TFont);
begin
  DoFoldersFontChanged(AFont);
end;

procedure TPreferencesForm.GolangFontSelectorSelectedFontChanged(Sender: TObject; AFont: TFont);
begin
  DoGolangFontChanged(AFont);
end;

function TPreferencesForm.GetColorConfig: TcaSynConfig;
begin
  Result := ColorGrid.ColorConfig;
end;

procedure TPreferencesForm.ColorGridChangedEvent(Sender: TObject);
begin
  DoColorFieldChanged;
end;

procedure TPreferencesForm.SetColorConfig(AValue: TcaSynConfig);
begin
  ColorGrid.ColorConfig := AValue;
end;

procedure TPreferencesForm.DoFilesFontChanged(AFont: TFont);
begin
  if Assigned(FOnFilesFontChanged) then FOnFilesFontChanged(Self, AFont);
end;

procedure TPreferencesForm.DoFoldersFontChanged(AFont: TFont);
begin
  if Assigned(FOnFoldersFontChanged) then FOnFoldersFontChanged(Self, AFont);
end;

procedure TPreferencesForm.DoGolangFontChanged(AFont: TFont);
begin
  if Assigned(FOnGolangFontChanged) then FOnGolangFontChanged(Self, AFont);
end;

procedure TPreferencesForm.DoColorFieldChanged;
begin
  if Assigned(FOnColorGridChanged) then FOnColorGridChanged(Self);
end;

end.

