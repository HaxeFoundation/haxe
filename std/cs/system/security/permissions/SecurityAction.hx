package cs.system.security.permissions;

/** Specifies the security actions that can be performed using declarative security. */
@:native("System.Security.Permissions.SecurityAction")
extern enum SecurityAction {
	Assert;
	Demand;
	Deny;
	InheritanceDemand;
	LinkDemand;
	PermitOnly;
	RequestMinimum;
	RequestOptional;
	RequestRefuse;
}
