package cs.system;

/** The exception that is thrown when an attempt is made to access an element of an array or collection with an index that is outside its bounds. */
@:native("System.IndexOutOfRangeException")
extern class IndexOutOfRangeException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
