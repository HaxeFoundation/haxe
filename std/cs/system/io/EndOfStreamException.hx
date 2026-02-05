package cs.system.io;

/** The exception that is thrown when reading is attempted past the end of a stream. */
@:native("System.IO.EndOfStreamException")
extern class EndOfStreamException extends cs.system.io.IOException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
