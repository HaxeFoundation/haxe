package cs.system.diagnostics.tracing;

/** Specifies additional event schema information for an event. */
@:native("System.Diagnostics.Tracing.EventAttribute")
extern class EventAttribute extends cs.system.Attribute {
	/**
	 * Specifies the behavior of the start and stop events of an activity. An activity
	 * is the region of time in an app between the start and the stop.
	 * @return Returns .
	 */
	var ActivityOptions(default, default):cs.system.diagnostics.tracing.EventActivityOptions;
	/**
	 * Gets or sets an additional event log where the event should be written.
	 * @return An additional event log where the event should be written.
	 */
	var Channel(default, default):cs.system.diagnostics.tracing.EventChannel;
	/**
	 * Gets or sets the identifier for the event.
	 * @return The event identifier. This value should be between 0 and 65535.
	 */
	var EventId(default, never):Int;
	/**
	 * Gets or sets the keywords for the event.
	 * @return A bitwise combination of the enumeration values.
	 */
	var Keywords(default, default):cs.system.diagnostics.tracing.EventKeywords;
	/**
	 * Gets or sets the level for the event.
	 * @return One of the enumeration values that specifies the level for the event.
	 */
	var Level(default, default):cs.system.diagnostics.tracing.EventLevel;
	/**
	 * Gets or sets the message for the event.
	 * @return The message for the event.
	 */
	var Message(default, default):String;
	/**
	 * Gets or sets the operation code for the event.
	 * @return One of the enumeration values that specifies the operation code.
	 */
	var Opcode(default, default):cs.system.diagnostics.tracing.EventOpcode;
	/**
	 * Gets or sets the  value for this  object. An event tag is a user-defined value
	 * that is passed through when the event is logged.
	 * @return The  value for this  object. An event tag is a user-defined value that
	 * is passed through when the event is logged.
	 */
	var Tags(default, default):cs.system.diagnostics.tracing.EventTags;
	/**
	 * Gets or sets the task for the event.
	 * @return The task for the event.
	 */
	var Task(default, default):cs.system.diagnostics.tracing.EventTask;
	/**
	 * Gets or sets the version of the event.
	 * @return The version of the event.
	 */
	var Version(default, default):cs.UInt8;
	function new(eventId:Int):Void;
}
