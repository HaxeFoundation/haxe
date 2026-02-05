package cs.system.data;

/** Represents the exception that is thrown when you call the  method within the  event. */
@:native("System.Data.InRowChangingEventException")
extern class InRowChangingEventException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
