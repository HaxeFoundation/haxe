package cs.system.threading;

/** Propagates notification that operations should be canceled. */
@:native("System.Threading.CancellationToken")
extern class CancellationToken extends cs.system.ValueType {
	/**
	 * Returns an empty  value.
	 * @return An empty cancellation token.
	 */
	static var None(default, never):cs.system.threading.CancellationToken;
	/**
	 * Gets whether this token is capable of being in the canceled state.
	 * @return if this token is capable of being in the canceled state; otherwise, .
	 */
	var CanBeCanceled(default, never):Bool;
	/**
	 * Gets whether cancellation has been requested for this token.
	 * @return if cancellation has been requested for this token; otherwise, .
	 */
	var IsCancellationRequested(default, never):Bool;
	/**
	 * Gets a  that is signaled when the token is canceled.
	 * @return A  that is signaled when the token is canceled.
	 */
	var WaitHandle(default, never):cs.system.threading.WaitHandle;
	function new(canceled:Bool):Void;
	/**
	 * Determines whether two  instances are equal.
	 * @param left The first instance.
	 * @param right The second instance.
	 * @return if the instances are equal; otherwise,  See the Remarks section for more
	 * information.
	 */
	static function op_Equality(left:cs.system.threading.CancellationToken, right:cs.system.threading.CancellationToken):Bool;
	/**
	 * Determines whether two  instances are not equal.
	 * @param left The first instance.
	 * @param right The second instance.
	 * @return if the instances are not equal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.threading.CancellationToken, right:cs.system.threading.CancellationToken):Bool;
	@:overload(function(other:Dynamic):Bool {})
	/**
	 * Determines whether the current  instance is equal to the specified .
	 * @param other The other object to compare with this instance.
	 * @return if  is a  and if the two instances are equal; otherwise, . See the
	 * Remarks section for more information.
	 */
	function Equals(other:cs.system.threading.CancellationToken):Bool;
	/**
	 * Serves as a hash function for a .
	 * @return A hash code for the current  instance.
	 */
	function GetHashCode():Int;
	@:overload(function(callback:cs.system.Action):cs.system.threading.CancellationTokenRegistration {})
	@:overload(function(callback:cs.system.Action, useSynchronizationContext:Bool):cs.system.threading.CancellationTokenRegistration {})
	@:overload(function(callback:cs.system.Action_1<Dynamic>, state:Dynamic):cs.system.threading.CancellationTokenRegistration {})
	/**
	 * Registers a delegate that will be called when this  is canceled.
	 * @param callback The delegate to be executed when the  is canceled.
	 * @return The  instance that can be used to unregister the callback.
	 */
	function Register(callback:cs.system.Action_1<Dynamic>, state:Dynamic, useSynchronizationContext:Bool):cs.system.threading.CancellationTokenRegistration;
	/** Throws a  if this token has had cancellation requested. */
	function ThrowIfCancellationRequested():Void;
}
