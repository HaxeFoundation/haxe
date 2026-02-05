package cs.system.io;

/** The exception thrown when the internal buffer overflows. */
@:native("System.IO.InternalBufferOverflowException")
extern class InternalBufferOverflowException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
