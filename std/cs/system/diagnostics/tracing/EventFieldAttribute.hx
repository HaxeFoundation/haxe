package cs.system.diagnostics.tracing;

/** The  is placed on fields of user-defined types that are passed as  payloads. */
@:native("System.Diagnostics.Tracing.EventFieldAttribute")
extern class EventFieldAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the value that specifies how to format the value of a user-defined
	 * type.
	 * @return The value that specifies how to format the value of a user-defined type.
	 */
	var Format(default, default):cs.system.diagnostics.tracing.EventFieldFormat;
	/**
	 * Gets or sets the user-defined  value that is required for fields that contain
	 * data that isn't one of the supported types.
	 * @return Returns .
	 */
	var Tags(default, default):cs.system.diagnostics.tracing.EventFieldTags;
	function new():Void;
}
