package cs.system.reflection;

/** Instructs obfuscation tools to take the specified actions for an assembly, type, or member. */
@:native("System.Reflection.ObfuscationAttribute")
extern class ObfuscationAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets a  value indicating whether the attribute of a type is to apply to
	 * the members of the type.
	 * @return if the attribute is to apply to the members of the type; otherwise, .
	 * The default is .
	 */
	var ApplyToMembers(default, default):Bool;
	/**
	 * Gets or sets a  value indicating whether the obfuscation tool should exclude the
	 * type or member from obfuscation.
	 * @return if the type or member to which this attribute is applied should be
	 * excluded from obfuscation; otherwise, . The default is .
	 */
	var Exclude(default, default):Bool;
	/**
	 * Gets or sets a string value that is recognized by the obfuscation tool, and
	 * which specifies processing options.
	 * @return A string value that is recognized by the obfuscation tool, and which
	 * specifies processing options. The default is "all".
	 */
	var Feature(default, default):String;
	/**
	 * Gets or sets a  value indicating whether the obfuscation tool should remove this
	 * attribute after processing.
	 * @return if an obfuscation tool should remove the attribute after processing;
	 * otherwise, . The default is .
	 */
	var StripAfterObfuscation(default, default):Bool;
	function new():Void;
}
