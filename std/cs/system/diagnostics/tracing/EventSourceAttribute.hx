package cs.system.diagnostics.tracing;

/** Allows the event tracing for Windows (ETW) name to be defined independently of the name of the event source class. */
@:native("System.Diagnostics.Tracing.EventSourceAttribute")
extern class EventSourceAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the event source identifier.
	 * @return The event source identifier.
	 */
	var Guid(default, default):String;
	/**
	 * Gets or sets the name of the localization resource file.
	 * @return The name of the localization resource file, or  if the localization
	 * resource file does not exist.
	 */
	var LocalizationResources(default, default):String;
	/**
	 * Gets or sets the name of the event source.
	 * @return The name of the event source.
	 */
	var Name(default, default):String;
	function new():Void;
}
