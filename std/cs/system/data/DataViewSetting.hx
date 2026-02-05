package cs.system.data;

/** Represents the default settings for , , , , , and  for DataViews created from the . */
@:native("System.Data.DataViewSetting")
extern class DataViewSetting {
	/**
	 * Gets or sets a value indicating whether to use the default sort.
	 * @return if the default sort is used; otherwise .
	 */
	var ApplyDefaultSort(default, default):Bool;
	/**
	 * Gets the  that contains this .
	 * @return A  object.
	 */
	var DataViewManager(default, never):cs.system.data.DataViewManager;
	/**
	 * Gets or sets the filter to apply in the . See  for a code sample using
	 * RowFilter.
	 * @return A string that contains the filter to apply.
	 */
	var RowFilter(default, default):String;
	/**
	 * Gets or sets a value indicating whether to display Current, Deleted, Modified
	 * Current, ModifiedOriginal, New, Original, Unchanged, or no rows in the .
	 * @return A value that indicates which rows to display.
	 */
	var RowStateFilter(default, default):cs.system.data.DataViewRowState;
	/**
	 * Gets or sets a value indicating the sort to apply in the .
	 * @return The sort to apply in the .
	 */
	var Sort(default, default):String;
	/**
	 * Gets the  to which the  properties apply.
	 * @return A  object.
	 */
	var Table(default, never):cs.system.data.DataTable;
}
