package cs.system.threading.tasks;

/** Provides an awaitable result of an asynchronous operation. */
@:native("System.Threading.Tasks.ValueTask")
extern class ValueTask extends cs.system.ValueType {
	/**
	 * Gets a value that indicates whether this object represents a canceled operation.
	 * @return if this object represents a canceled operation; otherwise, .
	 */
	var IsCanceled(default, never):Bool;
	/**
	 * Gets a value that indicates whether this object represents a completed
	 * operation.
	 * @return if this object represents a completed operation; otherwise, .
	 */
	var IsCompleted(default, never):Bool;
	/**
	 * Gets a value that indicates whether this object represents a successfully
	 * completed operation.
	 * @return if this object represents a successfully completed operation; otherwise,
	 * .
	 */
	var IsCompletedSuccessfully(default, never):Bool;
	/**
	 * Gets a value that indicates whether this object represents a failed operation.
	 * @return if this object represents a failed operation; otherwise, .
	 */
	var IsFaulted(default, never):Bool;
	@:overload(function(task:cs.system.threading.tasks.Task):Void {})
	function new(source:cs.system.threading.tasks.sources.IValueTaskSource, token:cs.Int16):Void;
	/**
	 * Compares two  values for equality.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return if the two  values are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.threading.tasks.ValueTask, right:cs.system.threading.tasks.ValueTask):Bool;
	/**
	 * Determines whether two  values are unequal.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return if the two  values are not equal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.threading.tasks.ValueTask, right:cs.system.threading.tasks.ValueTask):Bool;
	/**
	 * Retrieves a  object that represents this .
	 * @return The  object that is wrapped in this  if one exists, or a new  object
	 * that represents the result.
	 */
	function AsTask():cs.system.threading.tasks.Task;
	/**
	 * Configures an awaiter for this value.
	 * @param continueOnCapturedContext to attempt to marshal the continuation back to
	 * the captured context; otherwise, .
	 * @return The configured awaiter.
	 */
	function ConfigureAwait(continueOnCapturedContext:Bool):cs.system.runtime.compilerservices.ConfiguredValueTaskAwaitable;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Determines whether the specified object is equal to the current  instance.
	 * @param obj The object to compare with the current object.
	 * @return if the specified object is equal to the current object; otherwise, .
	 */
	function Equals(other:cs.system.threading.tasks.ValueTask):Bool;
	/**
	 * Creates an awaiter for this value.
	 * @return The awaiter.
	 */
	function GetAwaiter():cs.system.runtime.compilerservices.ValueTaskAwaiter;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code for the current object.
	 */
	function GetHashCode():Int;
	/**
	 * Gets a  that may be used at any point in the future.
	 * @return The preserved .
	 */
	function Preserve():cs.system.threading.tasks.ValueTask;
}
