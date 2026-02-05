package cs.system.security.principal;

/** Specifies how principal and identity objects should be created for an application domain. The default is . */
@:native("System.Security.Principal.PrincipalPolicy")
extern enum PrincipalPolicy {
	NoPrincipal;
	UnauthenticatedPrincipal;
	WindowsPrincipal;
}
