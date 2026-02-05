package cs.system.data;

/** Occurs when a target and source  have the same primary key value, and the  property is set to true. */
@:native("System.Data.MergeFailedEventArgs")
extern class MergeFailedEventArgs extends cs.system.EventArgs {
	/**
	 * Returns a description of the merge conflict.
	 * @return A description of the merge conflict.
	 */
	var Conflict(default, never):String;
	/**
	 * Returns the  object.
	 * @return The  object.
	 */
	var Table(default, never):cs.system.data.DataTable;
	function new(table:cs.system.data.DataTable, conflict:String):Void;
}
