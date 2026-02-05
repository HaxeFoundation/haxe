package cs.system.data;

/** Represents the exception that is thrown when you try to change the value of a read-only column. */
@:native("System.Data.ReadOnlyException")
extern class ReadOnlyException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
