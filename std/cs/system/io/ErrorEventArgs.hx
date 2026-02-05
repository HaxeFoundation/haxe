package cs.system.io;

/** Provides data for the  event. */
@:native("System.IO.ErrorEventArgs")
extern class ErrorEventArgs extends cs.system.EventArgs {
	function new(exception:cs.system.Exception):Void;
	/**
	 * Gets the  that represents the error that occurred.
	 * @return An  that represents the error that occurred.
	 */
	function GetException():cs.system.Exception;
}
