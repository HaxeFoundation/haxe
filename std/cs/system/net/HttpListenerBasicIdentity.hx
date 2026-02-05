package cs.system.net;

/** Holds the user name and password from a basic authentication request. */
@:native("System.Net.HttpListenerBasicIdentity")
extern class HttpListenerBasicIdentity extends cs.system.security.principal.GenericIdentity {
	/**
	 * Indicates the password from a basic authentication attempt.
	 * @return A  that holds the password.
	 */
	var Password(default, never):String;
	function new(username:String, password:String):Void;
}
