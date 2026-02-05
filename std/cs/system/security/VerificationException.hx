package cs.system.security;

/** The exception that is thrown when the security policy requires code to be type safe and the verification process is unable to verify that the code is type safe. */
@:native("System.Security.VerificationException")
extern class VerificationException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
