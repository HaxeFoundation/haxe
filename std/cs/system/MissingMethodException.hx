package cs.system;

/** The exception that is thrown when there is an attempt to dynamically access a method that does not exist. */
@:native("System.MissingMethodException")
extern class MissingMethodException extends cs.system.MissingMemberException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(className:String, methodName:String):Void;
}
