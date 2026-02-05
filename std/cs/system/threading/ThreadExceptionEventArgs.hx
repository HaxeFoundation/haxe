package cs.system.threading;

/** Provides data for the  event. */
@:native("System.Threading.ThreadExceptionEventArgs")
extern class ThreadExceptionEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the  that occurred.
	 * @return The  that occurred.
	 */
	var Exception(default, never):cs.system.Exception;
	function new(t:cs.system.Exception):Void;
}
