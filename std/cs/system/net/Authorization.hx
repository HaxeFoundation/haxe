package cs.system.net;

/** Contains an authentication message for an Internet server. */
@:native("System.Net.Authorization")
extern class Authorization {
	/**
	 * Gets the completion status of the authorization.
	 * @return if the authentication process is complete; otherwise, .
	 */
	var Complete(default, never):Bool;
	/**
	 * Gets a unique identifier for user-specific connections.
	 * @return A unique string that associates a connection with an authenticating
	 * entity.
	 */
	var ConnectionGroupId(default, never):String;
	/**
	 * Gets the message returned to the server in response to an authentication
	 * challenge.
	 * @return The message that will be returned to the server in response to an
	 * authentication challenge.
	 */
	var Message(default, never):String;
	/**
	 * Gets or sets a  value that indicates whether mutual authentication occurred.
	 * @return if both client and server were authenticated; otherwise, .
	 */
	var MutuallyAuthenticated(default, default):Bool;
	/**
	 * Gets or sets the prefix for Uniform Resource Identifiers (URIs) that can be
	 * authenticated with the  property.
	 * @return An array of strings that contains URI prefixes.
	 */
	var ProtectionRealm(default, default):cs.NativeArray<String>;
	@:overload(function(token:String):Void {})
	@:overload(function(token:String, finished:Bool):Void {})
	function new(token:String, finished:Bool, connectionGroupId:String):Void;
}
