package cs.system.threading;

/** The exception that is thrown when one thread acquires a  object that another thread has abandoned by exiting without releasing it. */
@:native("System.Threading.AbandonedMutexException")
extern class AbandonedMutexException extends cs.system.SystemException {
	/**
	 * Gets the abandoned mutex that caused the exception, if known.
	 * @return A  object that represents the abandoned mutex, or  if the abandoned
	 * mutex could not be identified.
	 */
	var Mutex(default, never):cs.system.threading.Mutex;
	/**
	 * Gets the index of the abandoned mutex that caused the exception, if known.
	 * @return The index, in the array of wait handles passed to the  method, of the 
	 * object that represents the abandoned mutex, or -1 if the index of the abandoned
	 * mutex could not be determined.
	 */
	var MutexIndex(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(location:Int, handle:cs.system.threading.WaitHandle):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	@:overload(function(message:String, location:Int, handle:cs.system.threading.WaitHandle):Void {})
	function new(message:String, inner:cs.system.Exception, location:Int, handle:cs.system.threading.WaitHandle):Void;
}
