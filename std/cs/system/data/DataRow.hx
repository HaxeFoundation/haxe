package cs.system.data;

/** Represents a row of data in a . */
@:native("System.Data.DataRow")
extern class DataRow {
	/**
	 * Gets a value that indicates whether there are errors in a row.
	 * @return if the row contains an error; otherwise, .
	 */
	var HasErrors(default, never):Bool;
	/**
	 * Gets or sets all the values for this row through an array.
	 * @return An array of type .
	 */
	var ItemArray(default, default):cs.NativeArray<Dynamic>;
	/**
	 * Gets or sets the custom error description for a row.
	 * @return The text describing an error.
	 */
	var RowError(default, default):String;
	/**
	 * Gets the current state of the row with regard to its relationship to the .
	 * @return One of the  values.
	 */
	var RowState(default, never):cs.system.data.DataRowState;
	/**
	 * Gets the  for which this row has a schema.
	 * @return The  to which this row belongs.
	 */
	var Table(default, never):cs.system.data.DataTable;
	@:overload(function(index0:cs.system.data.DataColumn):Dynamic {})
	@:overload(function(index0:cs.system.data.DataColumn, index1:cs.system.data.DataRowVersion):Dynamic {})
	@:overload(function(index0:Int):Dynamic {})
	@:overload(function(index0:Int, index1:cs.system.data.DataRowVersion):Dynamic {})
	@:overload(function(index0:String):Dynamic {})
	@:native("get_Item")
	function get_Item(index0:String, index1:cs.system.data.DataRowVersion):Dynamic;
	@:overload(function(index0:cs.system.data.DataColumn, value:Dynamic):Void {})
	@:overload(function(index0:Int, value:Dynamic):Void {})
	@:native("set_Item")
	function set_Item(index0:String, value:Dynamic):Void;
	/** Commits all the changes made to this row since the last time  was called. */
	function AcceptChanges():Void;
	/** Starts an edit operation on a  object. */
	function BeginEdit():Void;
	/** Cancels the current edit on the row. */
	function CancelEdit():Void;
	/** Clears the errors for the row. This includes the  and errors set with . */
	function ClearErrors():Void;
	/** Deletes the . */
	function Delete():Void;
	/** Ends the edit occurring on the row. */
	function EndEdit():Void;
	@:overload(function(relation:cs.system.data.DataRelation):cs.NativeArray<cs.system.data.DataRow> {})
	@:overload(function(relationName:String):cs.NativeArray<cs.system.data.DataRow> {})
	@:overload(function(relation:cs.system.data.DataRelation, version:cs.system.data.DataRowVersion):cs.NativeArray<cs.system.data.DataRow> {})
	/**
	 * Gets the child rows of this  using the specified .
	 * @param relation The  to use.
	 * @return An array of  objects or an array of length zero.
	 */
	function GetChildRows(relationName:String, version:cs.system.data.DataRowVersion):cs.NativeArray<cs.system.data.DataRow>;
	@:overload(function(column:cs.system.data.DataColumn):String {})
	@:overload(function(columnIndex:Int):String {})
	/**
	 * Gets the error description of the specified .
	 * @param column A .
	 * @return The text of the error description.
	 */
	function GetColumnError(columnName:String):String;
	/**
	 * Gets an array of columns that have errors.
	 * @return An array of  objects that contain errors.
	 */
	function GetColumnsInError():cs.NativeArray<cs.system.data.DataColumn>;
	@:overload(function(relation:cs.system.data.DataRelation):cs.system.data.DataRow {})
	@:overload(function(relationName:String):cs.system.data.DataRow {})
	@:overload(function(relation:cs.system.data.DataRelation, version:cs.system.data.DataRowVersion):cs.system.data.DataRow {})
	/**
	 * Gets the parent row of a  using the specified .
	 * @param relation The  to use.
	 * @return The parent  of the current row.
	 */
	function GetParentRow(relationName:String, version:cs.system.data.DataRowVersion):cs.system.data.DataRow;
	@:overload(function(relation:cs.system.data.DataRelation):cs.NativeArray<cs.system.data.DataRow> {})
	@:overload(function(relationName:String):cs.NativeArray<cs.system.data.DataRow> {})
	@:overload(function(relation:cs.system.data.DataRelation, version:cs.system.data.DataRowVersion):cs.NativeArray<cs.system.data.DataRow> {})
	/**
	 * Gets the parent rows of a  using the specified .
	 * @param relation The  to use.
	 * @return An array of  objects or an array of length zero.
	 */
	function GetParentRows(relationName:String, version:cs.system.data.DataRowVersion):cs.NativeArray<cs.system.data.DataRow>;
	/**
	 * Gets a value that indicates whether a specified version exists.
	 * @param version One of the  values that specifies the row version.
	 * @return if the version exists; otherwise, .
	 */
	function HasVersion(version:cs.system.data.DataRowVersion):Bool;
	@:overload(function(column:cs.system.data.DataColumn):Bool {})
	@:overload(function(columnIndex:Int):Bool {})
	@:overload(function(columnName:String):Bool {})
	/**
	 * Gets a value that indicates whether the specified  contains a null value.
	 * @param column A .
	 * @return if the column contains a null value; otherwise, .
	 */
	function IsNull(column:cs.system.data.DataColumn, version:cs.system.data.DataRowVersion):Bool;
	/** Rejects all changes made to the row since  was last called. */
	function RejectChanges():Void;
	/** Changes the  of a  to . */
	function SetAdded():Void;
	@:overload(function(column:cs.system.data.DataColumn, error:String):Void {})
	@:overload(function(columnIndex:Int, error:String):Void {})
	/**
	 * Sets the error description for a column specified as a .
	 * @param column The  to set the error description for.
	 * @param error The error description.
	 */
	function SetColumnError(columnName:String, error:String):Void;
	/** Changes the  of a  to . */
	function SetModified():Void;
	@:overload(function(parentRow:cs.system.data.DataRow):Void {})
	/**
	 * Sets the parent row of a  with specified new parent .
	 * @param parentRow The new parent .
	 */
	function SetParentRow(parentRow:cs.system.data.DataRow, relation:cs.system.data.DataRelation):Void;
}
