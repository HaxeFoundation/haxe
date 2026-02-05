package cs.system.diagnostics;

/** Identifies a switch used in an assembly, class, or member. */
@:native("System.Diagnostics.SwitchAttribute")
extern class SwitchAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the description of the switch.
	 * @return The description of the switch.
	 */
	var SwitchDescription(default, default):String;
	/**
	 * Gets or sets the display name of the switch.
	 * @return The display name of the switch.
	 */
	var SwitchName(default, default):String;
	/**
	 * Gets or sets the type of the switch.
	 * @return The type of the switch.
	 */
	var SwitchType(default, default):cs.system.Type;
	function new(switchName:String, switchType:cs.system.Type):Void;
	/**
	 * Returns all switch attributes for the specified assembly.
	 * @param assembly The assembly to check for switch attributes.
	 * @return An array that contains all the switch attributes for the assembly.
	 */
	static function GetAll(assembly:cs.system.reflection.Assembly):cs.NativeArray<cs.system.diagnostics.SwitchAttribute>;
}
