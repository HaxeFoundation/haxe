package cs.system.reflection;

/** The exception that is thrown in  when the filter criteria is not valid for the type of filter you are using. */
@:native("System.Reflection.InvalidFilterCriteriaException")
extern class InvalidFilterCriteriaException extends cs.system.ApplicationException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
