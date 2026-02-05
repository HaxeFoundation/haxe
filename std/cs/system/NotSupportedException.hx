package cs.system;

/** The exception that is thrown when an invoked method is not supported, or when there is an attempt to read, seek, or write to a stream that does not support the invoked functionality. */
@:native("System.NotSupportedException")
extern class NotSupportedException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
