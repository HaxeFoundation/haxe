package cs.system.data;

/** Represents the exception that is thrown when you try to insert a null value into a column where  is set to . */
@:native("System.Data.NoNullAllowedException")
extern class NoNullAllowedException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
