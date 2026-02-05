package cs.system.security.principal;

/** Defines security impersonation levels. Security impersonation levels govern the degree to which a server process can act on behalf of a client process. */
@:native("System.Security.Principal.TokenImpersonationLevel")
extern enum abstract TokenImpersonationLevel(Int) {
	var Anonymous = 1;
	var Delegation = 4;
	var Identification = 2;
	var Impersonation = 3;
	var None = 0;
}
