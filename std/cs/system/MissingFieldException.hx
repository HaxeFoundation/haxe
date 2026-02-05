package cs.system;

/** The exception that is thrown when there is an attempt to dynamically access a field that does not exist. If a field in a class library has been removed or renamed, recompile any assemblies that reference that library. */
@:native("System.MissingFieldException")
extern class MissingFieldException extends cs.system.MissingMemberException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(className:String, fieldName:String):Void;
}
