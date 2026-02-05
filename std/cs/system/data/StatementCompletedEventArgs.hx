package cs.system.data;

/** Provides additional information for the  event. */
@:native("System.Data.StatementCompletedEventArgs")
extern class StatementCompletedEventArgs extends cs.system.EventArgs {
	/**
	 * Indicates the number of rows affected by the statement that caused the  event to
	 * occur.
	 * @return The number of rows affected.
	 */
	var RecordCount(default, never):Int;
	function new(recordCount:Int):Void;
}
