package cs.system.diagnostics.contracts;

/** Enables you to set contract and tool options at assembly, type, or method granularity. */
@:native("System.Diagnostics.Contracts.ContractOptionAttribute")
extern class ContractOptionAttribute extends cs.system.Attribute {
	/**
	 * Gets the category of the option.
	 * @return The category of the option.
	 */
	var Category(default, never):String;
	/**
	 * Determines if an option is enabled.
	 * @return if the option is enabled; otherwise, .
	 */
	var Enabled(default, never):Bool;
	/**
	 * Gets the setting for the option.
	 * @return The setting for the option.
	 */
	var Setting(default, never):String;
	/**
	 * Gets the value for the option.
	 * @return The value for the option.
	 */
	var Value(default, never):String;
	@:overload(function(category:String, setting:String, enabled:Bool):Void {})
	function new(category:String, setting:String, value:String):Void;
}
