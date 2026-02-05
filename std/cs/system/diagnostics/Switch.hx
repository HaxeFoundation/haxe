package cs.system.diagnostics;

/** Provides an abstract base class to create new debugging and tracing switches. */
@:native("System.Diagnostics.Switch")
extern class Switch {
	/**
	 * Gets the custom switch attributes defined in the application configuration file.
	 * @return A  containing the case-insensitive custom attributes for the trace
	 * switch.
	 */
	var Attributes(default, never):cs.system.collections.specialized.StringDictionary;
	/**
	 * Gets a description of the switch.
	 * @return The description of the switch. The default value is an empty string
	 * ("").
	 */
	var Description(default, never):String;
	/**
	 * Gets a name used to identify the switch.
	 * @return The name used to identify the switch. The default value is an empty
	 * string ("").
	 */
	var DisplayName(default, never):String;
	/**
	 * Gets or sets the current setting for this switch.
	 * @return The current setting for this switch. The default is zero.
	 */
	var SwitchSetting(default, default):Int;
	/**
	 * Gets or sets the value of the switch.
	 * @return A string representing the value of the switch.
	 */
	var Value(default, default):String;
}
