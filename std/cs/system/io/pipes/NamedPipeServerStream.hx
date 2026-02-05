package cs.system.io.pipes;

/** Exposes a  around a named pipe, supporting both synchronous and asynchronous read and write operations. */
@:native("System.IO.Pipes.NamedPipeServerStream")
extern class NamedPipeServerStream extends cs.system.io.pipes.PipeStream {
	/** Represents the maximum number of server instances that the system resources allow. */
	static var MaxAllowedServerInstances(default, never):Int;
	@:overload(function(pipeName:String):Void {})
	@:overload(function(pipeName:String, direction:cs.system.io.pipes.PipeDirection):Void {})
	@:overload(function(pipeName:String, direction:cs.system.io.pipes.PipeDirection, maxNumberOfServerInstances:Int):Void {})
	@:overload(function(direction:cs.system.io.pipes.PipeDirection, isAsync:Bool, isConnected:Bool, safePipeHandle:cs.microsoft.win32.safehandles.SafePipeHandle):Void {})
	@:overload(function(pipeName:String, direction:cs.system.io.pipes.PipeDirection, maxNumberOfServerInstances:Int, transmissionMode:cs.system.io.pipes.PipeTransmissionMode):Void {})
	@:overload(function(pipeName:String, direction:cs.system.io.pipes.PipeDirection, maxNumberOfServerInstances:Int, transmissionMode:cs.system.io.pipes.PipeTransmissionMode, options:cs.system.io.pipes.PipeOptions):Void {})
	function new(pipeName:String, direction:cs.system.io.pipes.PipeDirection, maxNumberOfServerInstances:Int, transmissionMode:cs.system.io.pipes.PipeTransmissionMode, options:cs.system.io.pipes.PipeOptions, inBufferSize:Int, outBufferSize:Int):Void;
	/**
	 * Begins an asynchronous operation to wait for a client to connect.
	 * @param callback The method to call when a client connects to the  object.
	 * @param state A user-provided object that distinguishes this particular
	 * asynchronous request from other requests.
	 * @return An object that references the asynchronous request.
	 */
	function BeginWaitForConnection(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/** Disconnects the current connection. */
	function Disconnect():Void;
	/**
	 * Ends an asynchronous operation to wait for a client to connect.
	 * @param asyncResult The pending asynchronous request.
	 */
	function EndWaitForConnection(asyncResult:cs.system.IAsyncResult):Void;
	/**
	 * Gets the user name of the client on the other end of the pipe.
	 * @return The user name of the client on the other end of the pipe.
	 */
	function GetImpersonationUserName():String;
	/**
	 * Calls a delegate while impersonating the client.
	 * @param impersonationWorker The delegate that specifies a method to call.
	 */
	function RunAsClient(impersonationWorker:cs.system.io.pipes.PipeStreamImpersonationWorker):Void;
	/** Waits for a client to connect to this  object. */
	function WaitForConnection():Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously waits for a client to connect to this  object.
	 * @return A task that represents the asynchronous wait operation.
	 */
	function WaitForConnectionAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
