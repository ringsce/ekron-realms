{$mode objfpc}
{$modeswitch objectivec1}
{$linkframework CoreAudio}
{$linkframework AudioToolbox}

unit snd_mac;

interface

uses
  MacOSAll, SysUtils, Math;  // Math is required for Sin()

const
  SAMPLE_RATE = 44100;
  BUFFER_SIZE = 4096; // Size of each audio buffer
  NUM_BUFFERS = 3;    // Number of audio buffers

type
  TAudioQueueCallback = procedure (
    inUserData: Pointer;
    inAQ: AudioQueueRef;
    inBuffer: AudioQueueBufferRef
  ); mwpascal;  // Fixed calling convention

var
  AudioQueue: AudioQueueRef;
  Buffers: array[0..NUM_BUFFERS-1] of AudioQueueBufferRef;

procedure InitAudio;

implementation

procedure AudioCallback(
  inUserData: Pointer;
  inAQ: AudioQueueRef;
  inBuffer: AudioQueueBufferRef
); mwpascal;  // Fixed calling convention
var
  i: Integer;
  BufferData: PByte;
begin
  if inBuffer = nil then
    Exit; // Prevent null pointer access

  BufferData := PByte(inBuffer^.mAudioData);

  // Fill buffer with dummy sine wave data (example)
  for i := 0 to BUFFER_SIZE - 1 do
    BufferData[i] := Trunc(127 * Sin(2 * Pi * i / 100) + 128);

  // Set buffer properties
  inBuffer^.mAudioDataByteSize := BUFFER_SIZE;

  // Enqueue buffer for playback
  if AudioQueueEnqueueBuffer(inAQ, inBuffer, 0, nil) <> 0 then
    WriteLn('Failed to enqueue buffer');
end;

procedure InitAudio;
var
  Format: AudioStreamBasicDescription;
  i: Integer;
begin
  // Set up audio format (PCM)
  FillChar(Format, SizeOf(Format), 0);
  Format.mSampleRate := SAMPLE_RATE;
  Format.mFormatID := kAudioFormatLinearPCM;
  Format.mFormatFlags := kLinearPCMFormatFlagIsSignedInteger or kLinearPCMFormatFlagIsPacked;
  Format.mBitsPerChannel := 8;
  Format.mChannelsPerFrame := 1;
  Format.mBytesPerFrame := 1;
  Format.mBytesPerPacket := 1;
  Format.mFramesPerPacket := 1;

  // Create audio queue
if AudioQueueNewOutput(Format, @AudioCallback, nil, nil, nil, 0, @AudioQueue) <> 0 then
begin
  WriteLn('Failed to create audio queue');
  Exit;
end;

// Allocate and enqueue buffers
for i := 0 to NUM_BUFFERS - 1 do
begin
  if AudioQueueAllocateBuffer(AudioQueue, BUFFER_SIZE, @Buffers[i]) <> 0 then
  begin
    WriteLn('Failed to allocate buffer');
    Exit;
  end;
  AudioCallback(nil, AudioQueue, Buffers[i]); // Pre-fill buffer
end;

  // Start playback
  AudioQueueStart(AudioQueue, nil);
end;

initialization
  WriteLn('Initializing CoreAudio...');
  InitAudio;
  WriteLn('Playing sound...');

finalization
  AudioQueueStop(AudioQueue, True);
  AudioQueueDispose(AudioQueue, True);
  WriteLn('Audio stopped.');

end.

