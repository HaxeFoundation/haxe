package cs.system.security;

/** Specifies the scope of a . */
@:native("System.Security.SecurityCriticalScope")
extern enum abstract SecurityCriticalScope(Int) {
	var Everything = 1;
	var Explicit = 0;
}
