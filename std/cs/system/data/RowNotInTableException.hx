package cs.system.data;

/** Represents the exception that is thrown when you try to perform an operation on a  that is not in a . */
@:native("System.Data.RowNotInTableException")
extern class RowNotInTableException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
