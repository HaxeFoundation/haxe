package cs.system.security;

/** Specifies the default partial-trust visibility for code that is marked with the  (APTCA) attribute. */
@:native("System.Security.PartialTrustVisibilityLevel")
extern enum PartialTrustVisibilityLevel {
	NotVisibleByDefault;
	VisibleToAllHosts;
}
