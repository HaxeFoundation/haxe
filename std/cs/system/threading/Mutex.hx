package cs.system.threading;

/** A synchronization primitive that can also be used for interprocess synchronization. */
@:native("System.Threading.Mutex")
extern class Mutex extends cs.system.threading.WaitHandle {
	@:overload(function():Void {})
	@:overload(function(initiallyOwned:Bool):Void {})
	@:overload(function(initiallyOwned:Bool, name:String):Void {})
	function new(initiallyOwned:Bool, name:String, createdNew:cs.Ref<Bool>):Void;
	/**
	 * Opens the specified named mutex, if it already exists.
	 * @param name The name of the system mutex to open.
	 * @return An object that represents the named system mutex.
	 */
	static function OpenExisting(name:String):cs.system.threading.Mutex;
	/**
	 * Opens the specified named mutex, if it already exists, and returns a value that
	 * indicates whether the operation succeeded.
	 * @param name The name of the system mutex to open.
	 * @param result When this method returns, contains a  object that represents the
	 * named mutex if the call succeeded, or  if the call failed. This parameter is
	 * treated as uninitialized.
	 * @return if the named mutex was opened successfully; otherwise, .
	 */
	static function TryOpenExisting(name:String, result:cs.Ref<cs.system.threading.Mutex>):Bool;
	/** Releases the  once. */
	function ReleaseMutex():Void;
}
