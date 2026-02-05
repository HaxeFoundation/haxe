package cs.system.data;

/** Represents the exception that is thrown when incorrectly trying to create or access a relation. */
@:native("System.Data.InvalidConstraintException")
extern class InvalidConstraintException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
