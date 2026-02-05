package cs.system.threading;

/** The exception that is thrown when an attempt is made to open a system mutex, semaphore, or event wait handle that does not exist. */
@:native("System.Threading.WaitHandleCannotBeOpenedException")
extern class WaitHandleCannotBeOpenedException extends cs.system.ApplicationException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
