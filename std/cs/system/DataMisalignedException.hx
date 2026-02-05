package cs.system;

/** The exception that is thrown when a unit of data is read from or written to an address that is not a multiple of the data size. This class cannot be inherited. */
@:native("System.DataMisalignedException")
extern class DataMisalignedException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
