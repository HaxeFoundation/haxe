package cs.system;

/** The exception that is thrown when an attempt to access a class member fails. */
@:native("System.MemberAccessException")
extern class MemberAccessException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
