package cs.system.reflection;

/** Specifies that the assembly is not fully signed when created. */
@:native("System.Reflection.AssemblyDelaySignAttribute")
extern class AssemblyDelaySignAttribute extends cs.system.Attribute {
	/**
	 * Gets a value indicating the state of the attribute.
	 * @return if this assembly has been built as delay-signed; otherwise, .
	 */
	var DelaySign(default, never):Bool;
	function new(delaySign:Bool):Void;
}
