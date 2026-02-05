package cs.system.net.networkinformation;

/** The exception that is thrown when a  or  method calls a method that throws an exception. */
@:native("System.Net.NetworkInformation.PingException")
extern class PingException extends cs.system.InvalidOperationException {
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
