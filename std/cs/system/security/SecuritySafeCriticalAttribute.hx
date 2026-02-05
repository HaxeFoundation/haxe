package cs.system.security;

/** Identifies types or members as security-critical and safely accessible by transparent code. */
@:native("System.Security.SecuritySafeCriticalAttribute")
extern class SecuritySafeCriticalAttribute extends cs.system.Attribute {
	function new():Void;
}
