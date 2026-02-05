package cs.system.security.permissions;

/** Specifies the security actions that can be performed using declarative security. */
@:native("System.Security.Permissions.SecurityAction")
extern enum abstract SecurityAction(Int) {
	var Assert = 3;
	var Demand = 2;
	var Deny = 4;
	var InheritanceDemand = 7;
	var LinkDemand = 6;
	var PermitOnly = 5;
	var RequestMinimum = 8;
	var RequestOptional = 9;
	var RequestRefuse = 10;
}
