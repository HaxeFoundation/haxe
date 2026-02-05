package cs.system.security.authentication.extendedprotection;

/** The  enumeration specifies when the  should be enforced. */
@:native("System.Security.Authentication.ExtendedProtection.PolicyEnforcement")
extern enum PolicyEnforcement {
	Always;
	Never;
	WhenSupported;
}
