package cs.system.io;

/** The exception that is thrown when a path or fully qualified file name is longer than the system-defined maximum length. */
@:native("System.IO.PathTooLongException")
extern class PathTooLongException extends cs.system.io.IOException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
