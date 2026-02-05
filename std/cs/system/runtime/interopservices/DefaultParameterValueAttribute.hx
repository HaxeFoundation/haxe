package cs.system.runtime.interopservices;

/** Sets the default value of a parameter when called from a language that supports default parameters. This class cannot be inherited. */
@:native("System.Runtime.InteropServices.DefaultParameterValueAttribute")
extern class DefaultParameterValueAttribute extends cs.system.Attribute {
	/**
	 * Gets the default value of a parameter.
	 * @return An object that represents the default value of a parameter.
	 */
	var Value(default, never):Dynamic;
	function new(value:Dynamic):Void;
}
