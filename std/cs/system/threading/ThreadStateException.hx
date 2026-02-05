package cs.system.threading;

/** The exception that is thrown when a  is in an invalid  for the method call. */
@:native("System.Threading.ThreadStateException")
extern class ThreadStateException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
