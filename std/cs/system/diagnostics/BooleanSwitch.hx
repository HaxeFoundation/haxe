package cs.system.diagnostics;

/** Provides a simple on/off switch that controls debugging and tracing output. */
@:native("System.Diagnostics.BooleanSwitch")
extern class BooleanSwitch extends cs.system.diagnostics.Switch {
	/**
	 * Gets or sets a value indicating whether the switch is enabled or disabled.
	 * @return if the switch is enabled; otherwise, . The default is .
	 */
	var Enabled(default, default):Bool;
	@:overload(function(displayName:String, description:String):Void {})
	function new(displayName:String, description:String, defaultSwitchValue:String):Void;
}
