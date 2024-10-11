unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef iOS}
  Interfaces,  // Required for the Lazarus LCL to work on iOS
  iOSAll,      // For iOS-specific functionality like file paths
  {$endif}
  {$ifdef Darwin}
  MacOSAll,    // For macOS-specific functions
  {$endif}
  Forms,
  Graphics,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  SysUtils,
  Classes,
  Dialogs;  // Use this for both macOS and iOS

type
  TForm1 = class(TForm)
  private
    Image: TImage;
    ProgressBar: TProgressBar;
    DownloadButton: TButton;
    StatusLabel: TLabel;
    procedure OnDownloadClick(Sender: TObject);
    function GetImagePath: String;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width := 600;
  Height := 400;
  Caption := 'Game Download System';

  // Create and configure TImage for the photo
  Image := TImage.Create(Self);
  Image.Parent := Self;
  Image.SetBounds(10, 10, 300, 300);
  Image.Stretch := True;  // Make the image fit the bounds

  // Load image from 'images' folder relative to the executable
  try
    Image.Picture.LoadFromFile(GetImagePath + 'ringsce.png'); // Ensure this file exists inside the app bundle
  except
    on E: Exception do
      ShowMessage('Image could not be loaded: ' + E.Message);
  end;

  // Create and configure TProgressBar for the download status
  ProgressBar := TProgressBar.Create(Self);
  ProgressBar.Parent := Self;
  ProgressBar.SetBounds(10, 320, 580, 30);
  ProgressBar.Min := 0;
  ProgressBar.Max := 100;
  ProgressBar.Position := 0;

  // Create a Label for status updates
  StatusLabel := TLabel.Create(Self);
  StatusLabel.Parent := Self;
  StatusLabel.SetBounds(10, 360, 200, 30);
  StatusLabel.Caption := 'Download Status: 0%';

  // Create a Button to start the download
  DownloadButton := TButton.Create(Self);
  DownloadButton.Parent := Self;
  DownloadButton.SetBounds(450, 360, 140, 30);
  DownloadButton.Caption := 'Start Download';
  DownloadButton.OnClick := @OnDownloadClick;
end;

procedure TForm1.OnDownloadClick(Sender: TObject);
var
  i: Integer;
begin
  // Simulate download progress (on iOS, avoid Sleep on the main thread)
  for i := 0 to 100 do
  begin
    Sleep(50);  // Simulate download time, avoid on main thread in production apps for iOS
    ProgressBar.Position := i;
    StatusLabel.Caption := Format('Download Status: %d%%', [i]);
    Application.ProcessMessages; // Update the UI during the loop
  end;
  StatusLabel.Caption := 'Download Complete!';
end;

// Function to get the path to the images folder for macOS and iOS
function TForm1.GetImagePath: String;
begin
  {$ifdef iOS}
  // iOS: Use the bundle resource path
  Result := NSBundle.mainBundle.resourcePath.UTF8String + PathDelim + 'images' + PathDelim;
  {$endif}
  {$ifdef Darwin}
  // macOS: Use the app's resources folder
  Result := ExtractFilePath(ParamStr(0)) + 'images' + PathDelim;
  {$endif}
end;

begin
  Application.Initialize;
  Form1 := TForm1.Create(nil);
  Application.Run;
end.

