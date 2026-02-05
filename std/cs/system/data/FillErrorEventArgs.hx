package cs.system.data;

/** Provides data for the  event of a . */
@:native("System.Data.FillErrorEventArgs")
extern class FillErrorEventArgs extends cs.system.EventArgs {
	/**
	 * Gets or sets a value indicating whether to continue the fill operation despite
	 * the error.
	 * @return if the fill operation should continue; otherwise, .
	 */
	var Continue(default, default):Bool;
	/**
	 * Gets the  being updated when the error occurred.
	 * @return The  being updated.
	 */
	var DataTable(default, never):cs.system.data.DataTable;
	/**
	 * Gets the errors being handled.
	 * @return The errors being handled.
	 */
	var Errors(default, default):cs.system.Exception;
	/**
	 * Gets the values for the row being updated when the error occurred.
	 * @return The values for the row being updated.
	 */
	var Values(default, never):cs.NativeArray<Dynamic>;
	function new(dataTable:cs.system.data.DataTable, values:cs.NativeArray<Dynamic>):Void;
}
