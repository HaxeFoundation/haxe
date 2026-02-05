package cs.system.data;

/** The exception that is thrown by a strongly typed  when the user accesses a  value. */
@:native("System.Data.StrongTypingException")
extern class StrongTypingException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(s:String, innerException:cs.system.Exception):Void;
}
