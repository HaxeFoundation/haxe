package cs.system.diagnostics;

/** Provides data for the  and  events. */
@:native("System.Diagnostics.DataReceivedEventArgs")
extern class DataReceivedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the line of characters that was written to a redirected  output stream.
	 * @return The line that was written by an associated  to its redirected  or 
	 * stream.
	 */
	var Data(default, never):String;
}
