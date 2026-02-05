package cs.system.data;

/** Represents the exception that is thrown when errors are generated using ADO.NET components. */
@:native("System.Data.DataException")
extern class DataException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(s:String, innerException:cs.system.Exception):Void;
}
