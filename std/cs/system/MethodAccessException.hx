package cs.system;

/** The exception that is thrown when there is an invalid attempt to access a method, such as accessing a private method from partially trusted code. */
@:native("System.MethodAccessException")
extern class MethodAccessException extends cs.system.MemberAccessException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
