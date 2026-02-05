package cs.system.threading;

/** The exception that is thrown when a  is interrupted while it is in a waiting state. */
@:native("System.Threading.ThreadInterruptedException")
extern class ThreadInterruptedException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
