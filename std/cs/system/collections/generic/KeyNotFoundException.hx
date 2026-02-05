package cs.system.collections.generic;

/** The exception that is thrown when the key specified for accessing an element in a collection does not match any key in the collection. */
@:native("System.Collections.Generic.KeyNotFoundException")
extern class KeyNotFoundException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
