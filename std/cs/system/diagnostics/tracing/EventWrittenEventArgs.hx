package cs.system.diagnostics.tracing;

/** Provides data for the  callback. */
@:native("System.Diagnostics.Tracing.EventWrittenEventArgs")
extern class EventWrittenEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the activity ID on the thread that the event was written to.
	 * @return The activity ID on the thread that the event was written to.
	 */
	var ActivityId(default, never):cs.system.Guid;
	/**
	 * Gets the channel for the event.
	 * @return The channel for the event.
	 */
	var Channel(default, never):cs.system.diagnostics.tracing.EventChannel;
	/**
	 * Gets the event identifier.
	 * @return The event identifier.
	 */
	var EventId(default, never):Int;
	/**
	 * Gets the name of the event.
	 * @return The name of the event.
	 */
	var EventName(default, never):String;
	/**
	 * Gets the event source object.
	 * @return The event source object.
	 */
	var EventSource(default, never):cs.system.diagnostics.tracing.EventSource;
	/**
	 * Gets the keywords for the event.
	 * @return The keywords for the event.
	 */
	var Keywords(default, never):cs.system.diagnostics.tracing.EventKeywords;
	/**
	 * Gets the level of the event.
	 * @return The level of the event.
	 */
	var Level(default, never):cs.system.diagnostics.tracing.EventLevel;
	/**
	 * Gets the message for the event.
	 * @return The message for the event.
	 */
	var Message(default, never):String;
	/**
	 * Gets the operation code for the event.
	 * @return The operation code for the event.
	 */
	var Opcode(default, never):cs.system.diagnostics.tracing.EventOpcode;
	var OSThreadId(default, never):haxe.Int64;
	/**
	 * Gets the payload for the event.
	 * @return The payload for the event.
	 */
	var Payload(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<Dynamic>;
	/**
	 * Returns a list of strings that represent the property names of the event.
	 * @return Returns .
	 */
	var PayloadNames(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<String>;
	/**
	 * Gets the identifier of an activity that is related to the activity represented
	 * by the current instance.
	 * @return The identifier of the related activity, or  if there is no related
	 * activity.
	 */
	var RelatedActivityId(default, never):cs.system.Guid;
	/**
	 * Returns the tags specified in the call to the  method.
	 * @return Returns .
	 */
	var Tags(default, never):cs.system.diagnostics.tracing.EventTags;
	/**
	 * Gets the task for the event.
	 * @return The task for the event.
	 */
	var Task(default, never):cs.system.diagnostics.tracing.EventTask;
	var TimeStamp(default, never):cs.system.DateTime;
	/**
	 * Gets the version of the event.
	 * @return The version of the event.
	 */
	var Version(default, never):cs.UInt8;
}
