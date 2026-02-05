package cs.system.security.authentication;

/** The exception that is thrown when authentication fails for an authentication stream. */
@:native("System.Security.Authentication.AuthenticationException")
extern class AuthenticationException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
