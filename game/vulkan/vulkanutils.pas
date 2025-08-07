unit VulkanUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Vulkan;

function LoadVulkanLibrary: Boolean;
procedure UnloadVulkanLibrary;
function CreateVulkanInstance(out Instance: VkInstance): Boolean;
function CheckVulkanSupport: Boolean;

implementation

var
  VulkanLibHandle: THandle = 0;

function LoadVulkanLibrary: Boolean;
begin
  Result := False;
  {$IFDEF MSWINDOWS}
  VulkanLibHandle := LoadLibrary('vulkan-1.dll');
  {$ELSE}
  VulkanLibHandle := LoadLibrary('libvulkan.so');
  {$ENDIF}

  if VulkanLibHandle = 0 then
  begin
    WriteLn('Error: Could not load Vulkan library.');
    Exit(False);
  end;

  if not InitVulkan then
  begin
    WriteLn('Error: Vulkan initialization failed.');
    FreeLibrary(VulkanLibHandle);
    VulkanLibHandle := 0;
    Exit(False);
  end;

  WriteLn('Vulkan library loaded successfully.');
  Result := True;
end;

procedure UnloadVulkanLibrary;
begin
  if VulkanLibHandle <> 0 then
  begin
    FreeLibrary(VulkanLibHandle);
    VulkanLibHandle := 0;
  end;
end;

function CreateVulkanInstance(out Instance: VkInstance): Boolean;
var
  AppInfo: VkApplicationInfo;
  CreateInfo: VkInstanceCreateInfo;
  Res: VkResult;
begin
  FillChar(AppInfo, SizeOf(AppInfo), 0);
  AppInfo.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
  AppInfo.pApplicationName := 'Quake2D';
  AppInfo.applicationVersion := VK_MAKE_VERSION(1, 0, 0);
  AppInfo.pEngineName := 'Quake2D Engine';
  AppInfo.engineVersion := VK_MAKE_VERSION(1, 0, 0);
  AppInfo.apiVersion := VK_API_VERSION_1_0;

  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  CreateInfo.pApplicationInfo := @AppInfo;

  Res := vkCreateInstance(@CreateInfo, nil, @Instance);
  if Res <> VK_SUCCESS then
  begin
    WriteLn('Error: Failed to create Vulkan instance.');
    Exit(False);
  end;

  WriteLn('Vulkan instance created successfully.');
  Result := True;
end;

function CheckVulkanSupport: Boolean;
var
  Instance: VkInstance;
begin
  if not LoadVulkanLibrary then
    Exit(False);

  Result := CreateVulkanInstance(Instance);
  if Result then
    vkDestroyInstance(Instance, nil);

  UnloadVulkanLibrary;
end;

initialization

finalization
  UnloadVulkanLibrary;

end.

