package cs.system.io.pipes;

/** Exposes the client side of an anonymous pipe stream, which supports both synchronous and asynchronous read and write operations. */
@:native("System.IO.Pipes.AnonymousPipeClientStream")
extern class AnonymousPipeClientStream extends cs.system.io.pipes.PipeStream {
	@:overload(function(pipeHandleAsString:String):Void {})
	@:overload(function(direction:cs.system.io.pipes.PipeDirection, safePipeHandle:cs.microsoft.win32.safehandles.SafePipeHandle):Void {})
	function new(direction:cs.system.io.pipes.PipeDirection, pipeHandleAsString:String):Void;
}
