package cs.system;

/** The exception that is thrown in a thread upon cancellation of an operation that the thread was executing. */
@:native("System.OperationCanceledException")
extern class OperationCanceledException extends cs.system.SystemException {
	/**
	 * Gets a token associated with the operation that was canceled.
	 * @return A token associated with the operation that was canceled, or a default
	 * token.
	 */
	var CancellationToken(default, never):cs.system.threading.CancellationToken;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(token:cs.system.threading.CancellationToken):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	@:overload(function(message:String, token:cs.system.threading.CancellationToken):Void {})
	function new(message:String, innerException:cs.system.Exception, token:cs.system.threading.CancellationToken):Void;
}
