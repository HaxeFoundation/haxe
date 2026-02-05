package cs.system.data;

/** Represents a databindable, customized view of a  for sorting, filtering, searching, editing, and navigation. The  does not store data, but instead represents a connected view of its corresponding . Changes to the 's data will affect the . Changes to the 's data will affect all s associated with it. */
@:native("System.Data.DataView")
extern class DataView extends cs.system.componentmodel.MarshalByValueComponent {
	/**
	 * Sets or gets a value that indicates whether deletes are allowed.
	 * @return , if deletes are allowed; otherwise, .
	 */
	var AllowDelete(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether edits are allowed.
	 * @return , if edits are allowed; otherwise, .
	 */
	var AllowEdit(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether the new rows can be added by using
	 * the  method.
	 * @return , if new rows can be added; otherwise, .
	 */
	var AllowNew(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether to use the default sort. The default
	 * sort is (ascending) by all primary keys as specified by .
	 * @return , if the default sort is used; otherwise, .
	 */
	var ApplyDefaultSort(default, default):Bool;
	/**
	 * Gets the number of records in the  after  and  have been applied.
	 * @return The number of records in the .
	 */
	var Count(default, never):Int;
	/**
	 * Gets the  associated with this view.
	 * @return The  that created this view. If this is the default  for a , the 
	 * property returns the default  for the . Otherwise, if the  was created without a
	 * , this property is .
	 */
	var DataViewManager(default, never):cs.system.data.DataViewManager;
	/**
	 * Gets a value that indicates whether the component is initialized.
	 * @return to indicate the component has completed initialization; otherwise, .
	 */
	var IsInitialized(default, never):Bool;
	/**
	 * Gets a value that indicates whether the data source is currently open and
	 * projecting views of data on the .
	 * @return , if the source is open; otherwise, .
	 */
	var IsOpen(default, never):Bool;
	/**
	 * Gets or sets the expression used to filter which rows are viewed in the .
	 * @return A string that specifies how rows are to be filtered.
	 */
	var RowFilter(default, default):String;
	/**
	 * Gets or sets the row state filter used in the .
	 * @return One of the  values.
	 */
	var RowStateFilter(default, default):cs.system.data.DataViewRowState;
	/**
	 * Gets or sets the sort column or columns, and sort order for the .
	 * @return A string that contains the column name followed by "ASC" (ascending) or
	 * "DESC" (descending). Columns are sorted ascending by default. Multiple columns
	 * can be separated by commas.
	 */
	var Sort(default, default):String;
	/**
	 * Gets or sets the source .
	 * @return A  that provides the data for this view.
	 */
	var Table(default, default):cs.system.data.DataTable;
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.data.DataRowView;
	@:overload(function():Void {})
	@:overload(function(table:cs.system.data.DataTable):Void {})
	function new(table:cs.system.data.DataTable, RowFilter:String, Sort:String, RowState:cs.system.data.DataViewRowState):Void;
	/**
	 * Adds a new row to the .
	 * @return A new  object.
	 */
	function AddNew():cs.system.data.DataRowView;
	/** Starts the initialization of a  that is used on a form or used by another component. The initialization occurs at runtime. */
	function BeginInit():Void;
	/**
	 * Copies items into an array. Only for Web Forms Interfaces.
	 * @param array array to copy into.
	 * @param index index to start at.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Deletes a row at the specified index.
	 * @param index The index of the row to delete.
	 */
	function Delete(index:Int):Void;
	/** Ends the initialization of a  that is used on a form or used by another component. The initialization occurs at runtime. */
	function EndInit():Void;
	/**
	 * Determines whether the specified  instances are considered equal.
	 * @param view The  to be compared.
	 * @return if the two  instances are equal; otherwise, .
	 */
	function Equals(view:cs.system.data.DataView):Bool;
	@:overload(function(key:Dynamic):Int {})
	/**
	 * Finds a row in the  by the specified sort key value.
	 * @param key The object to search for.
	 * @return The index of the row in the  that contains the sort key value specified;
	 * otherwise -1 if the sort key value does not exist.
	 */
	function Find(key:cs.NativeArray<Dynamic>):Int;
	@:overload(function(key:Dynamic):cs.NativeArray<cs.system.data.DataRowView> {})
	/**
	 * Returns an array of  objects whose columns match the specified sort key value.
	 * @param key The column value, typed as , to search for.
	 * @return An array of  objects whose columns match the specified sort key value;
	 * or, if no rows contain the specified sort key values, an empty  array.
	 */
	function FindRows(key:cs.NativeArray<Dynamic>):cs.NativeArray<cs.system.data.DataRowView>;
	/**
	 * Gets an enumerator for this .
	 * @return An  for navigating through the list.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	@:overload(function():cs.system.data.DataTable {})
	@:overload(function(tableName:String):cs.system.data.DataTable {})
	@:overload(function(distinct:Bool, columnNames:cs.NativeArray<String>):cs.system.data.DataTable {})
	/**
	 * Creates and returns a new  based on rows in an existing .
	 * @return A new  instance that contains the requested rows and columns.
	 */
	function ToTable(tableName:String, distinct:Bool, columnNames:cs.NativeArray<String>):cs.system.data.DataTable;
}
