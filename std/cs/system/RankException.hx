package cs.system;

/** The exception that is thrown when an array with the wrong number of dimensions is passed to a method. */
@:native("System.RankException")
extern class RankException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
