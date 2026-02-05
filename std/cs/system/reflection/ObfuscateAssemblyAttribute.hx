package cs.system.reflection;

/** Instructs obfuscation tools to use their standard obfuscation rules for the appropriate assembly type. */
@:native("System.Reflection.ObfuscateAssemblyAttribute")
extern class ObfuscateAssemblyAttribute extends cs.system.Attribute {
	/**
	 * Gets a  value indicating whether the assembly was marked private.
	 * @return if the assembly was marked private; otherwise, .
	 */
	var AssemblyIsPrivate(default, never):Bool;
	/**
	 * Gets or sets a  value indicating whether the obfuscation tool should remove the
	 * attribute after processing.
	 * @return if the obfuscation tool should remove the attribute after processing;
	 * otherwise, . The default value for this property is .
	 */
	var StripAfterObfuscation(default, default):Bool;
	function new(assemblyIsPrivate:Bool):Void;
}
