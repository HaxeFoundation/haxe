package cs.system.threading;

/** The exception that is thrown when recursive entry into a lock is not compatible with the recursion policy for the lock. */
@:native("System.Threading.LockRecursionException")
extern class LockRecursionException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
