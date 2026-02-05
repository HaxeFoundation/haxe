package cs.system.security.permissions;

/** Specifies the base attribute class for declarative security from which  is derived. */
@:native("System.Security.Permissions.SecurityAttribute")
extern class SecurityAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets a security action.
	 * @return One of the  values.
	 */
	var Action(default, default):cs.system.security.permissions.SecurityAction;
	/**
	 * Gets or sets a value indicating whether full (unrestricted) permission to the
	 * resource protected by the attribute is declared.
	 * @return if full permission to the protected resource is declared; otherwise, .
	 */
	var Unrestricted(default, default):Bool;
	/**
	 * When overridden in a derived class, creates a permission object that can then be
	 * serialized into binary form and persistently stored along with the  in an
	 * assembly's metadata.
	 * @return A serializable permission object.
	 */
	function CreatePermission():cs.system.security.IPermission;
}
