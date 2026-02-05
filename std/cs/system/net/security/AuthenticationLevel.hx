package cs.system.net.security;

/** Specifies client requirements for authentication and impersonation when using the  class and derived classes to request a resource. */
@:native("System.Net.Security.AuthenticationLevel")
extern enum AuthenticationLevel {
	MutualAuthRequested;
	MutualAuthRequired;
	None;
}
