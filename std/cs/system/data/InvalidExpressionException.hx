package cs.system.data;

/** Represents the exception that is thrown when you try to add a  that contains an invalid  to a . */
@:native("System.Data.InvalidExpressionException")
extern class InvalidExpressionException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
