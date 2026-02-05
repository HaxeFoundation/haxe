package cs.system.io.pipes;

/** Exposes a stream around an anonymous pipe, which supports both synchronous and asynchronous read and write operations. */
@:native("System.IO.Pipes.AnonymousPipeServerStream")
extern class AnonymousPipeServerStream extends cs.system.io.pipes.PipeStream {
	/**
	 * Gets the safe handle for the  object that is currently connected to the  object.
	 * @return A handle for the  object that is currently connected to the  object.
	 */
	var ClientSafePipeHandle(default, never):cs.microsoft.win32.safehandles.SafePipeHandle;
	@:overload(function():Void {})
	@:overload(function(direction:cs.system.io.pipes.PipeDirection):Void {})
	@:overload(function(direction:cs.system.io.pipes.PipeDirection, inheritability:cs.system.io.HandleInheritability):Void {})
	@:overload(function(direction:cs.system.io.pipes.PipeDirection, serverSafePipeHandle:cs.microsoft.win32.safehandles.SafePipeHandle, clientSafePipeHandle:cs.microsoft.win32.safehandles.SafePipeHandle):Void {})
	function new(direction:cs.system.io.pipes.PipeDirection, inheritability:cs.system.io.HandleInheritability, bufferSize:Int):Void;
	/** Closes the local copy of the  object's handle. */
	function DisposeLocalCopyOfClientHandle():Void;
	/**
	 * Gets the connected  object's handle as a string.
	 * @return A string that represents the connected  object's handle.
	 */
	function GetClientHandleAsString():String;
}
