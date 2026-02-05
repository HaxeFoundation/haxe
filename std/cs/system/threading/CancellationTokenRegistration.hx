package cs.system.threading;

/** Represents a callback delegate that has been registered with a . */
@:native("System.Threading.CancellationTokenRegistration")
extern class CancellationTokenRegistration extends cs.system.ValueType {
	/**
	 * Gets the  with which this registration is associated.  If the registration isn't
	 * associated with a token (such as after the registration has been disposed), this
	 * will return a default token.
	 * @return The cancellation token with which this registration is associated, or a
	 * default token if the registration isn't associated with a token (such as after
	 * the registration has been disposed).
	 */
	var Token(default, never):cs.system.threading.CancellationToken;
	/**
	 * Determines whether two  instances are equal.
	 * @param left The first instance.
	 * @param right The second instance.
	 * @return True if the instances are equal; otherwise, false.
	 */
	static function op_Equality(left:cs.system.threading.CancellationTokenRegistration, right:cs.system.threading.CancellationTokenRegistration):Bool;
	/**
	 * Determines whether two  instances are not equal.
	 * @param left The first instance.
	 * @param right The second instance.
	 * @return True if the instances are not equal; otherwise, false.
	 */
	static function op_Inequality(left:cs.system.threading.CancellationTokenRegistration, right:cs.system.threading.CancellationTokenRegistration):Bool;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Disposes of the registration and unregisters the target callback from the
	 * associated .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Determines whether the current  instance is equal to the specified .
	 * @param obj The other object to which to compare this instance.
	 * @return True, if both this and  are equal. False, otherwise. Two  instances are
	 * equal if they both refer to the output of a single call to the same Register
	 * method of a .
	 */
	function Equals(other:cs.system.threading.CancellationTokenRegistration):Bool;
	/**
	 * Serves as a hash function for a .
	 * @return A hash code for the current  instance.
	 */
	function GetHashCode():Int;
}
