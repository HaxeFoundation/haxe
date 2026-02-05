package cs.system.data;

/** Provides data for the  method. */
@:native("System.Data.DataTableClearEventArgs")
extern class DataTableClearEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the table whose rows are being cleared.
	 * @return The  whose rows are being cleared.
	 */
	var Table(default, never):cs.system.data.DataTable;
	/**
	 * Gets the table name whose rows are being cleared.
	 * @return A  indicating the table name.
	 */
	var TableName(default, never):String;
	/**
	 * Gets the namespace of the table whose rows are being cleared.
	 * @return A  indicating the namespace name.
	 */
	var TableNamespace(default, never):String;
	function new(dataTable:cs.system.data.DataTable):Void;
}
