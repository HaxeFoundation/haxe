package cs.system.security.authentication;

/** The exception that is thrown when authentication fails for an authentication stream and cannot be retried. */
@:native("System.Security.Authentication.InvalidCredentialException")
extern class InvalidCredentialException extends cs.system.security.authentication.AuthenticationException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
