package cs.system.security;

/** Identifies the set of security rules the common language runtime should enforce for an assembly. */
@:native("System.Security.SecurityRuleSet")
extern enum SecurityRuleSet {
	Level1;
	Level2;
	None;
}
