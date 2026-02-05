package cs.system.security.principal;

/** Represents a generic principal. */
@:native("System.Security.Principal.GenericPrincipal")
extern class GenericPrincipal extends cs.system.security.claims.ClaimsPrincipal {
	function new(identity:cs.system.security.principal.IIdentity, roles:cs.NativeArray<String>):Void;
	/**
	 * Determines whether the current  belongs to the specified role.
	 * @param role The name of the role for which to check membership.
	 * @return if the current  is a member of the specified role; otherwise, .
	 */
	function IsInRole(role:String):Bool;
}
