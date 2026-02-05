package cs.system.security;

/** Allows an assembly to be called by partially trusted code. Without this declaration, only fully trusted callers are able to use the assembly. This class cannot be inherited. */
@:native("System.Security.AllowPartiallyTrustedCallersAttribute")
extern class AllowPartiallyTrustedCallersAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the default partial trust visibility for code that is marked with
	 * the  (APTCA) attribute.
	 * @return One of the enumeration values. The default is .
	 */
	var PartialTrustVisibilityLevel(default, default):cs.system.security.PartialTrustVisibilityLevel;
	function new():Void;
}
