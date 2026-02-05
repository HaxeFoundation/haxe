package cs.system.security;

/** Identifies the set of security rules the common language runtime should enforce for an assembly. */
@:native("System.Security.SecurityRuleSet")
extern enum abstract SecurityRuleSet(Int) {
	var Level1;
	var Level2;
	var None;
}
