package cs.system.data;

/** Represents the exception that is thrown when the  property of a  contains a syntax error. */
@:native("System.Data.SyntaxErrorException")
extern class SyntaxErrorException extends cs.system.data.InvalidExpressionException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
