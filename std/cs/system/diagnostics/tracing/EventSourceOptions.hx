package cs.system.diagnostics.tracing;

/** Specifies overrides of default event settings such as the log level, keywords and operation code when the  method is called. */
@:native("System.Diagnostics.Tracing.EventSourceOptions")
extern class EventSourceOptions extends cs.system.ValueType {
	/**
	 * The activity options defined for this event source.
	 * @return Returns .
	 */
	var ActivityOptions(default, default):cs.system.diagnostics.tracing.EventActivityOptions;
	/**
	 * Gets or sets the keywords applied to the event. If this property is not set, the
	 * event's keywords will be .
	 * @return The keywords applied to the event, or  if no keywords are set.
	 */
	var Keywords(default, default):cs.system.diagnostics.tracing.EventKeywords;
	/**
	 * Gets or sets the event level applied to the event.
	 * @return The event level for the event. If not set, the default is Verbose (5).
	 */
	var Level(default, default):cs.system.diagnostics.tracing.EventLevel;
	/**
	 * Gets or sets the operation code to use for the specified event.
	 * @return The operation code to use for the specified event. If not set, the
	 * default is  (0).
	 */
	var Opcode(default, default):cs.system.diagnostics.tracing.EventOpcode;
	/**
	 * The event tags defined for this event source.
	 * @return Returns .
	 */
	var Tags(default, default):cs.system.diagnostics.tracing.EventTags;
}
