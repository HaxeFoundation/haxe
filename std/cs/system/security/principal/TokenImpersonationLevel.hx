package cs.system.security.principal;

/** Defines security impersonation levels. Security impersonation levels govern the degree to which a server process can act on behalf of a client process. */
@:native("System.Security.Principal.TokenImpersonationLevel")
extern enum TokenImpersonationLevel {
	Anonymous;
	Delegation;
	Identification;
	Impersonation;
	None;
}
