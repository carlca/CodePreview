program CodePreviewInit;

{$mode objfpc}{$H+}

{$DEFINE XCODE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  cajsonconfig,
  LazUtf8,
  SysUtils
  { you can add units after this };

function GetConfigPath: string;
begin
  {$IFDEF XCODE}
  Result := '/users/carlca/Code/fpc/CodePreview/config.json';
  {$ELSE}
  Result := Utils.AppPath + 'config.json';
  {$ENDIF}
end;

procedure CreateConfig;
var
  C: TcaJsonConfig;
begin
  C := TcaJsonConfig.Create(nil);
  try
    C.FileName := GetConfigPath;
    // mainForm
    C.Group := 'mainForm';
    C.IntProp['left'] := 500;
    C.IntProp['top'] := 50;
    C.IntProp['width'] := 1000;
    C.IntProp['height'] := 600;
    // paths
    C.Group := 'paths';
    C.StrProp['gopath'] := GetEnvironmentVariableUtf8('GOPATH');
    C.StrProp['root'] := '';
    // files
    C.Group := 'files';
    C.IntProp['height'] := 250;
    // leftPanel
    C.Group := 'leftPanel';
    C.IntProp['width'] := 350;
  finally
    C.Free;
  end;
end;

begin
  CreateConfig;
end.

