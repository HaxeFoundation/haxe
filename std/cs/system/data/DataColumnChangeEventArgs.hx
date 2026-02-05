package cs.system.data;

/** Provides data for the  event. */
@:native("System.Data.DataColumnChangeEventArgs")
extern class DataColumnChangeEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the  with a changing value.
	 * @return The  with a changing value.
	 */
	var Column(default, never):cs.system.data.DataColumn;
	/**
	 * Gets or sets the proposed new value for the column.
	 * @return The proposed value, of type .
	 */
	var ProposedValue(default, default):Dynamic;
	/**
	 * Gets the  of the column with a changing value.
	 * @return The  of the column with a changing value.
	 */
	var Row(default, never):cs.system.data.DataRow;
	function new(row:cs.system.data.DataRow, column:cs.system.data.DataColumn, value:Dynamic):Void;
}
