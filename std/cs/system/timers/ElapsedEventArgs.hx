package cs.system.timers;

/** Provides data for the  event. */
@:native("System.Timers.ElapsedEventArgs")
extern class ElapsedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the date/time when the  event was raised.
	 * @return The time the  event was raised.
	 */
	var SignalTime(default, never):cs.system.DateTime;
}
