package cs.system.security;

/** Specifies that code or an assembly performs security-critical operations. */
@:native("System.Security.SecurityCriticalAttribute")
extern class SecurityCriticalAttribute extends cs.system.Attribute {
	/**
	 * Gets the scope for the attribute.
	 * @return One of the enumeration values that specifies the scope of the attribute.
	 * The default is , which indicates that the attribute applies only to the
	 * immediate target.
	 */
	var Scope(default, never):cs.system.security.SecurityCriticalScope;
	@:overload(function():Void {})
	function new(scope:cs.system.security.SecurityCriticalScope):Void;
}
