package cs.system.diagnostics.tracing;

/** Provides the arguments for the  callback. */
@:native("System.Diagnostics.Tracing.EventCommandEventArgs")
extern class EventCommandEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the array of arguments for the callback.
	 * @return An array of callback arguments.
	 */
	var Arguments(default, never):cs.system.collections.generic.IDictionary<String, String>;
	/**
	 * Gets the command for the callback.
	 * @return The callback command.
	 */
	var Command(default, never):cs.system.diagnostics.tracing.EventCommand;
	/**
	 * Disables the event that have the specified identifier.
	 * @param eventId The identifier of the event to disable.
	 * @return if  is in range; otherwise, .
	 */
	function DisableEvent(eventId:Int):Bool;
	/**
	 * Enables the event that has the specified identifier.
	 * @param eventId The identifier of the event to enable.
	 * @return if  is in range; otherwise, .
	 */
	function EnableEvent(eventId:Int):Bool;
}
