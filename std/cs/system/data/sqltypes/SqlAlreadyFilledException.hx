package cs.system.data.sqltypes;

/** The  class is not intended for use as a stand-alone component, but as a class from which other classes derive standard functionality. */
@:native("System.Data.SqlTypes.SqlAlreadyFilledException")
extern class SqlAlreadyFilledException extends cs.system.data.sqltypes.SqlTypeException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, e:cs.system.Exception):Void;
}
