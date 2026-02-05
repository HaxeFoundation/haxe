package cs.system.reflection.emit;

/** Creates or associates parameter information. */
@:native("System.Reflection.Emit.ParameterBuilder")
extern class ParameterBuilder {
	/**
	 * Retrieves the attributes for this parameter.
	 * @return Read-only. Retrieves the attributes for this parameter.
	 */
	var Attributes(default, never):Int;
	/**
	 * Retrieves whether this is an input parameter.
	 * @return Read-only. Retrieves whether this is an input parameter.
	 */
	var IsIn(default, never):Bool;
	/**
	 * Retrieves whether this parameter is optional.
	 * @return Read-only. Specifies whether this parameter is optional.
	 */
	var IsOptional(default, never):Bool;
	/**
	 * Retrieves whether this parameter is an output parameter.
	 * @return Read-only. Retrieves whether this parameter is an output parameter.
	 */
	var IsOut(default, never):Bool;
	/**
	 * Retrieves the name of this parameter.
	 * @return Read-only. Retrieves the name of this parameter.
	 */
	var Name(default, never):String;
	/**
	 * Retrieves the signature position for this parameter.
	 * @return Read-only. Retrieves the signature position for this parameter.
	 */
	var Position(default, never):Int;
	/**
	 * Sets the default value of the parameter.
	 * @param defaultValue The default value of this parameter.
	 */
	function SetConstant(defaultValue:Dynamic):Void;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Set a custom attribute using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attributes.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
}
