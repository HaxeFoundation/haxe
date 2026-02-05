package cs.system.diagnostics.tracing;

/** The exception that is thrown when an error occurs during event tracing for Windows (ETW). */
@:native("System.Diagnostics.Tracing.EventSourceException")
extern class EventSourceException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
