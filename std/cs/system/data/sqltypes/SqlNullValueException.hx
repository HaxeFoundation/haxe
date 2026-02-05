package cs.system.data.sqltypes;

/** The exception that is thrown when the  property of a  structure is set to null. */
@:native("System.Data.SqlTypes.SqlNullValueException")
extern class SqlNullValueException extends cs.system.data.sqltypes.SqlTypeException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, e:cs.system.Exception):Void;
}
