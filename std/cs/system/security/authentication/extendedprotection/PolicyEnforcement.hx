package cs.system.security.authentication.extendedprotection;

/** The  enumeration specifies when the  should be enforced. */
@:native("System.Security.Authentication.ExtendedProtection.PolicyEnforcement")
extern enum abstract PolicyEnforcement(Int) {
	var Always = 2;
	var Never = 0;
	var WhenSupported = 1;
}
