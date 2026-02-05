package cs.system.data;

/** Provides data for the , , , and  events. */
@:native("System.Data.DataRowChangeEventArgs")
extern class DataRowChangeEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the action that has occurred on a .
	 * @return One of the  values.
	 */
	var Action(default, never):cs.system.data.DataRowAction;
	/**
	 * Gets the row upon which an action has occurred.
	 * @return The  upon which an action has occurred.
	 */
	var Row(default, never):cs.system.data.DataRow;
	function new(row:cs.system.data.DataRow, action:cs.system.data.DataRowAction):Void;
}
