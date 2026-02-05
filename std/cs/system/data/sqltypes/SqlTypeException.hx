package cs.system.data.sqltypes;

/** The base exception class for the . */
@:native("System.Data.SqlTypes.SqlTypeException")
extern class SqlTypeException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, e:cs.system.Exception):Void;
}
