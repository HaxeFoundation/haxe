package cs.system.data;

/** Represents the exception that is thrown when attempting an action that violates a constraint. */
@:native("System.Data.ConstraintException")
extern class ConstraintException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
