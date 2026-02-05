package cs.system.security;

/** Identifies which of the nonpublic  members are accessible by transparent code within the assembly. */
@:native("System.Security.SecurityTreatAsSafeAttribute")
extern class SecurityTreatAsSafeAttribute extends cs.system.Attribute {
	function new():Void;
}
