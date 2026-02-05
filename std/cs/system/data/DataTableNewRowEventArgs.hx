package cs.system.data;

/** Provides data for the  method. */
@:native("System.Data.DataTableNewRowEventArgs")
extern class DataTableNewRowEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the row that is being added.
	 * @return The  that is being added.
	 */
	var Row(default, never):cs.system.data.DataRow;
	function new(dataRow:cs.system.data.DataRow):Void;
}
