package cs.system.security.principal;

/** Specifies how principal and identity objects should be created for an application domain. The default is . */
@:native("System.Security.Principal.PrincipalPolicy")
extern enum abstract PrincipalPolicy(Int) {
	var NoPrincipal = 1;
	var UnauthenticatedPrincipal = 0;
	var WindowsPrincipal = 2;
}
