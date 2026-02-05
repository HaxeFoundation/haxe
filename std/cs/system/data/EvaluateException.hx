package cs.system.data;

/** Represents the exception that is thrown when the  property of a  cannot be evaluated. */
@:native("System.Data.EvaluateException")
extern class EvaluateException extends cs.system.data.InvalidExpressionException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
