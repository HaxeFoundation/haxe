package cs.system.security;

/** Marks modules containing unverifiable code. This class cannot be inherited. */
@:native("System.Security.UnverifiableCodeAttribute")
extern class UnverifiableCodeAttribute extends cs.system.Attribute {
	function new():Void;
}
