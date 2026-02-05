package cs.system;

/** The exception that is thrown for invalid casting or explicit conversion. */
@:native("System.InvalidCastException")
extern class InvalidCastException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, errorCode:Int):Void;
}
