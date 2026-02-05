package cs.system;

/** The exception that is thrown when a time zone cannot be found. */
@:native("System.TimeZoneNotFoundException")
extern class TimeZoneNotFoundException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
