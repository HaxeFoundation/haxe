package cs.system.data;

/** Represents the exception that is thrown when an action is tried on a  that has been deleted. */
@:native("System.Data.DeletedRowInaccessibleException")
extern class DeletedRowInaccessibleException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
