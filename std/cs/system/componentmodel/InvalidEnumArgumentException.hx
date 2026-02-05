package cs.system.componentmodel;

/** The exception thrown when using invalid arguments that are enumerators. */
@:native("System.ComponentModel.InvalidEnumArgumentException")
extern class InvalidEnumArgumentException extends cs.system.ArgumentException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(argumentName:String, invalidValue:Int, enumClass:cs.system.Type):Void;
}
