package cs.system;

/** The exception that is thrown when time zone information is invalid. */
@:native("System.InvalidTimeZoneException")
extern class InvalidTimeZoneException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
