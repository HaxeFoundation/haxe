package cs.system.reflection;

/** The exception that is thrown when the binary format of a custom attribute is invalid. */
@:native("System.Reflection.CustomAttributeFormatException")
extern class CustomAttributeFormatException extends cs.system.FormatException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
