package cs.system.security;

/** Specifies that an assembly cannot cause an elevation of privilege. */
@:native("System.Security.SecurityTransparentAttribute")
extern class SecurityTransparentAttribute extends cs.system.Attribute {
	function new():Void;
}
