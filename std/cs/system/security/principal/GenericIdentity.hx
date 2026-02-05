package cs.system.security.principal;

/** Represents a generic user. */
@:native("System.Security.Principal.GenericIdentity")
extern class GenericIdentity extends cs.system.security.claims.ClaimsIdentity {
	@:overload(function(name:String):Void {})
	function new(name:String, type:String):Void;
	/**
	 * Creates a new object that is a copy of the current instance.
	 * @return A copy of the current instance.
	 */
	function Clone():cs.system.security.claims.ClaimsIdentity;
}
