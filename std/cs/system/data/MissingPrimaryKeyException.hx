package cs.system.data;

/** Represents the exception that is thrown when you try to access a row in a table that has no primary key. */
@:native("System.Data.MissingPrimaryKeyException")
extern class MissingPrimaryKeyException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
