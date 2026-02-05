package cs.system.io.pipes;

/** Exposes a  around a named pipe, which supports both synchronous and asynchronous read and write operations. */
@:native("System.IO.Pipes.NamedPipeClientStream")
extern class NamedPipeClientStream extends cs.system.io.pipes.PipeStream {
	/**
	 * Gets the number of server instances that share the same pipe name.
	 * @return The number of server instances that share the same pipe name.
	 */
	var NumberOfServerInstances(default, never):Int;
	@:overload(function(pipeName:String):Void {})
	@:overload(function(serverName:String, pipeName:String):Void {})
	@:overload(function(serverName:String, pipeName:String, direction:cs.system.io.pipes.PipeDirection):Void {})
	@:overload(function(direction:cs.system.io.pipes.PipeDirection, isAsync:Bool, isConnected:Bool, safePipeHandle:cs.microsoft.win32.safehandles.SafePipeHandle):Void {})
	@:overload(function(serverName:String, pipeName:String, direction:cs.system.io.pipes.PipeDirection, options:cs.system.io.pipes.PipeOptions):Void {})
	@:overload(function(serverName:String, pipeName:String, direction:cs.system.io.pipes.PipeDirection, options:cs.system.io.pipes.PipeOptions, impersonationLevel:cs.system.security.principal.TokenImpersonationLevel):Void {})
	function new(serverName:String, pipeName:String, direction:cs.system.io.pipes.PipeDirection, options:cs.system.io.pipes.PipeOptions, impersonationLevel:cs.system.security.principal.TokenImpersonationLevel, inheritability:cs.system.io.HandleInheritability):Void;
	@:overload(function():Void {})
	/** Connects to a waiting server with an infinite time-out value. */
	function Connect(timeout:Int):Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	@:overload(function(timeout:Int):cs.system.threading.tasks.Task {})
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously connects to a waiting server with an infinite timeout period.
	 * @return A task that represents the asynchronous connect operation.
	 */
	function ConnectAsync(timeout:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
