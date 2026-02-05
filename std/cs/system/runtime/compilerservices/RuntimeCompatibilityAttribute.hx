package cs.system.runtime.compilerservices;

/** Specifies whether to wrap exceptions that do not derive from the  class with a  object. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.RuntimeCompatibilityAttribute")
extern class RuntimeCompatibilityAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets a value that indicates whether to wrap exceptions that do not
	 * derive from the  class with a  object.
	 * @return if exceptions that do not derive from the  class should appear wrapped
	 * with a  object; otherwise, .
	 */
	var WrapNonExceptionThrows(default, default):Bool;
	function new():Void;
}
