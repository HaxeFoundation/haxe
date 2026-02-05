package cs.system.data.sqltypes;

/** The exception that is thrown when you set a value into a  structure would truncate that value. */
@:native("System.Data.SqlTypes.SqlTruncateException")
extern class SqlTruncateException extends cs.system.data.sqltypes.SqlTypeException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, e:cs.system.Exception):Void;
}
