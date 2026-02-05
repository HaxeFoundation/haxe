package cs.system.security.principal;

/** Defines the basic functionality of a principal object. */
@:native("System.Security.Principal.IPrincipal")
extern interface IPrincipal {
	/**
	 * Gets the identity of the current principal.
	 * @return The  object associated with the current principal.
	 */
	var Identity(default, never):cs.system.security.principal.IIdentity;
	/**
	 * Determines whether the current principal belongs to the specified role.
	 * @param role The name of the role for which to check membership.
	 * @return if the current principal is a member of the specified role; otherwise, .
	 */
	function IsInRole(role:String):Bool;
}
