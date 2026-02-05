package cs.system;

/** Indicates whether a program element is compliant with the Common Language Specification (CLS). This class cannot be inherited. */
@:native("System.CLSCompliantAttribute")
extern class CLSCompliantAttribute extends cs.system.Attribute {
	/**
	 * Gets the Boolean value indicating whether the indicated program element is
	 * CLS-compliant.
	 * @return if the program element is CLS-compliant; otherwise, .
	 */
	var IsCompliant(default, never):Bool;
	function new(isCompliant:Bool):Void;
}
