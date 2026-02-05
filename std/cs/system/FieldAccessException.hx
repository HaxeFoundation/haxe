package cs.system;

/** The exception that is thrown when there is an invalid attempt to access a private or protected field inside a class. */
@:native("System.FieldAccessException")
extern class FieldAccessException extends cs.system.MemberAccessException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
