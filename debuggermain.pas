unit DebuggerMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, StdCtrls;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    OpenItem: TMenuItem;
    ExitItem: TMenuItem;
    DebugMenu: TMenuItem;
    RunItem: TMenuItem;
    StepItem: TMenuItem;
    StopItem: TMenuItem;
    ListViewErrors: TListView;
    TabControl: TTabControl;
    OpenDialog: TOpenDialog;

    procedure FormCreate(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure RunItemClick(Sender: TObject);
    procedure StepItemClick(Sender: TObject);
    procedure StopItemClick(Sender: TObject);
  private
    procedure AddError(Line: Integer; ErrorType, Message: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Set up ListView columns
  with ListViewErrors.Columns.Add do begin
    Caption := 'Line';
    Width := 50;
  end;
  with ListViewErrors.Columns.Add do begin
    Caption := 'Error Type';
    Width := 100;
  end;
  with ListViewErrors.Columns.Add do begin
    Caption := 'Message';
    Width := 250;
  end;

  ListViewErrors.ViewStyle := vsReport;
  TabControl.Tabs.Add('Error Log');
  TabControl.Tabs.Add('Debug Output');
end;

procedure TMainForm.OpenItemClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    ShowMessage('Opened file: ' + OpenDialog.FileName);
end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.RunItemClick(Sender: TObject);
begin
  ShowMessage('Debugging started...');
  AddError(10, 'Syntax Error', 'Unexpected token "END"');
end;

procedure TMainForm.StepItemClick(Sender: TObject);
begin
  ShowMessage('Step executed.');
end;

procedure TMainForm.StopItemClick(Sender: TObject);
begin
  ShowMessage('Debugging stopped.');
end;

procedure TMainForm.AddError(Line: Integer; ErrorType, Message: string);
var
  Item: TListItem;
begin
  Item := ListViewErrors.Items.Add;
  Item.Caption := IntToStr(Line);
  Item.SubItems.Add(ErrorType);
  Item.SubItems.Add(Message);
end;

end.

