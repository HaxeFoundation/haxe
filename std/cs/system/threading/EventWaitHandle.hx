package cs.system.threading;

/** Represents a thread synchronization event. */
@:native("System.Threading.EventWaitHandle")
extern class EventWaitHandle extends cs.system.threading.WaitHandle {
	@:overload(function(initialState:Bool, mode:cs.system.threading.EventResetMode):Void {})
	@:overload(function(initialState:Bool, mode:cs.system.threading.EventResetMode, name:String):Void {})
	function new(initialState:Bool, mode:cs.system.threading.EventResetMode, name:String, createdNew:cs.Ref<Bool>):Void;
	/**
	 * Opens the specified named synchronization event, if it already exists.
	 * @param name The name of the system synchronization event to open.
	 * @return An  object that represents the named system event.
	 */
	static function OpenExisting(name:String):cs.system.threading.EventWaitHandle;
	/**
	 * Opens the specified named synchronization event, if it already exists, and
	 * returns a value that indicates whether the operation succeeded.
	 * @param name The name of the system synchronization event to open.
	 * @param result When this method returns, contains a  object that represents the
	 * named synchronization event if the call succeeded, or  if the call failed. This
	 * parameter is treated as uninitialized.
	 * @return if the named synchronization event was opened successfully; otherwise, .
	 */
	static function TryOpenExisting(name:String, result:cs.Ref<cs.system.threading.EventWaitHandle>):Bool;
	/**
	 * Sets the state of the event to nonsignaled, causing threads to block.
	 * @return if the operation succeeds; otherwise, .
	 */
	function Reset():Bool;
	/**
	 * Sets the state of the event to signaled, allowing one or more waiting threads to
	 * proceed.
	 * @return if the operation succeeds; otherwise, .
	 */
	function Set():Bool;
}
