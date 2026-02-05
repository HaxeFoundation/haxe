package cs.system.threading;

/** Signals to a  that it should be canceled. */
@:native("System.Threading.CancellationTokenSource")
extern class CancellationTokenSource {
	/**
	 * Gets whether cancellation has been requested for this .
	 * @return if cancellation has been requested for this ; otherwise, .
	 */
	var IsCancellationRequested(default, never):Bool;
	/**
	 * Gets the  associated with this .
	 * @return The  associated with this .
	 */
	var Token(default, never):cs.system.threading.CancellationToken;
	@:overload(function():Void {})
	@:overload(function(millisecondsDelay:Int):Void {})
	function new(delay:cs.system.TimeSpan):Void;
	@:overload(function(tokens:cs.NativeArray<cs.system.threading.CancellationToken>):cs.system.threading.CancellationTokenSource {})
	/**
	 * Creates a  that will be in the canceled state when any of the source tokens are
	 * in the canceled state.
	 * @param token1 The first cancellation token to observe.
	 * @param token2 The second cancellation token to observe.
	 * @return A  that is linked to the source tokens.
	 */
	static function CreateLinkedTokenSource(token1:cs.system.threading.CancellationToken, token2:cs.system.threading.CancellationToken):cs.system.threading.CancellationTokenSource;
	@:overload(function():Void {})
	/** Communicates a request for cancellation. */
	function Cancel(throwOnFirstException:Bool):Void;
	@:overload(function(millisecondsDelay:Int):Void {})
	/**
	 * Schedules a cancel operation on this  after the specified number of
	 * milliseconds.
	 * @param millisecondsDelay The time span to wait before canceling this .
	 */
	function CancelAfter(delay:cs.system.TimeSpan):Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
}
