unit download;

{$MODE ObjFPC}{$H+}

uses
  Interfaces, // Required for the Lazarus LCL to work
  Forms,
  Graphics,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  SysUtils,
  Classes;

type
  TGameForm = class(TForm)
  private
    Image: TImage;
    ProgressBar: TProgressBar;
    DownloadButton: TButton;
    StatusLabel: TLabel;
    procedure OnDownloadClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  GameForm: TGameForm;

{ TGameForm }

constructor TGameForm.Create(TheOwner: TComponent);
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
  Image.Picture.LoadFromFile('game_cover.jpg'); // Load a sample image (update with your own path)

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

procedure TGameForm.OnDownloadClick(Sender: TObject);
var
  i: Integer;
begin
  // Simulate download progress (you can replace this with real logic)
  for i := 0 to 100 do
  begin
    Sleep(50);  // Simulate download time
    ProgressBar.Position := i;
    StatusLabel.Caption := Format('Download Status: %d%%', [i]);
    Application.ProcessMessages; // Update the UI during the loop
  end;
  StatusLabel.Caption := 'Download Complete!';
end;

begin
  Application.Initialize;
  GameForm := TGameForm.Create(nil);
  Application.Run;
end.

