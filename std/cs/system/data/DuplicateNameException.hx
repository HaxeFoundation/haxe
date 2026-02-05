package cs.system.data;

/** Represents the exception that is thrown when a duplicate database object name is encountered during an add operation in a  -related object. */
@:native("System.Data.DuplicateNameException")
extern class DuplicateNameException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
