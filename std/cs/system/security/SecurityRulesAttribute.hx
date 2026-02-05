package cs.system.security;

/** Indicates the set of security rules the common language runtime should enforce for an assembly. */
@:native("System.Security.SecurityRulesAttribute")
extern class SecurityRulesAttribute extends cs.system.Attribute {
	/**
	 * Gets the rule set to be applied.
	 * @return One of the enumeration values that specifies the transparency rules to
	 * be applied.
	 */
	var RuleSet(default, never):cs.system.security.SecurityRuleSet;
	/**
	 * Determines whether fully trusted transparent code should skip Microsoft
	 * intermediate language (MSIL) verification.
	 * @return if MSIL verification should be skipped; otherwise, . The default is .
	 */
	var SkipVerificationInFullTrust(default, default):Bool;
	function new(ruleSet:cs.system.security.SecurityRuleSet):Void;
}
