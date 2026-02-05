package cs.system.data;

/** Represents the exception that is thrown when you try to return a version of a  that has been deleted. */
@:native("System.Data.VersionNotFoundException")
extern class VersionNotFoundException extends cs.system.data.DataException {
	@:overload(function():Void {})
	@:overload(function(s:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
