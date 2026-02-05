package cs.system.security.principal;

/** Defines the basic functionality of an identity object. */
@:native("System.Security.Principal.IIdentity")
extern interface IIdentity {
	/**
	 * Gets the type of authentication used.
	 * @return The type of authentication used to identify the user.
	 */
	var AuthenticationType(default, never):String;
	/**
	 * Gets a value that indicates whether the user has been authenticated.
	 * @return if the user was authenticated; otherwise, .
	 */
	var IsAuthenticated(default, never):Bool;
	/**
	 * Gets the name of the current user.
	 * @return The name of the user on whose behalf the code is running.
	 */
	var Name(default, never):String;
}
