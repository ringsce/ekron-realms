unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef iOS}
  Interfaces,  // Required for Lazarus on iOS
  iOSAll,      // UIKit and iOS-specific functionality
  {$endif}
  Forms,
  Graphics,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  SysUtils,
  Classes,
  Dialogs;  // Replace with iOS dialog if needed

type
  TForm1 = class(TForm)
  private
    Image: TImage;
    ProgressBar: TProgressBar;
    DownloadButton: TButton;
    StatusLabel: TLabel;
    DownloadProgress: Integer;
    Timer: NSTimer;  // Use NSTimer to simulate progress
    procedure OnDownloadClick(Sender: TObject);
    procedure UpdateProgress(timer: NSTimer);
    function GetImagePath: String;
    procedure ShowAlert(const AMessage: String); // iOS-specific alert
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
  Caption := 'Game Download System (iOS)';

  // Create and configure TImage for the photo
  Image := TImage.Create(Self);
  Image.Parent := Self;
  Image.SetBounds(10, 10, 300, 300);
  Image.Stretch := True;  // Make the image fit the bounds

  // Load image from the app bundle
  try
    Image.Picture.LoadFromFile(GetImagePath + 'ringsce.png');
  except
    on E: Exception do
      ShowAlert('Image could not be loaded: ' + E.Message);
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
begin
  DownloadProgress := 0;
  Timer := NSTimer.scheduledTimerWithTimeInterval(0.5, @UpdateProgress, True);
end;

procedure TForm1.UpdateProgress(timer: NSTimer);
begin
  if DownloadProgress < 100 then
  begin
    Inc(DownloadProgress);
    ProgressBar.Position := DownloadProgress;
    StatusLabel.Caption := Format('Download Status: %d%%', [DownloadProgress]);
  end
  else
  begin
    Timer.invalidate;  // Stop the timer
    StatusLabel.Caption := 'Download Complete!';
  end;
end;

function TForm1.GetImagePath: String;
begin
  {$ifdef iOS}
  Result := NSBundle.mainBundle.resourcePath.UTF8String + PathDelim + 'images' + PathDelim;
  {$else}
  Result := ExtractFilePath(ParamStr(0)) + 'images' + PathDelim;
  {$endif}
end;

// Show an iOS alert using UIAlertController
procedure TForm1.ShowAlert(const AMessage: String);
var
  alert: UIAlertController;
begin
  alert := UIAlertController.alertControllerWithTitle_message_preferredStyle('Error', AMessage, UIAlertControllerStyleAlert);
  alert.addAction(UIAlertAction.actionWithTitle_style_handler('OK', UIAlertActionStyleDefault, nil));
  // Present the alert using the iOS framework
  UIWindow.keyWindow.rootViewController.presentViewController_animated_completion(alert, True, nil);
end;

begin
  Application.Initialize;
  Form1 := TForm1.Create(nil);
  Application.Run;
end.

