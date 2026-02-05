package cs.system.data;

/** Contains a default  for each  in a . */
@:native("System.Data.DataViewManager")
extern class DataViewManager extends cs.system.componentmodel.MarshalByValueComponent {
	/**
	 * Gets or sets the  to use with the .
	 * @return The  to use.
	 */
	var DataSet(default, default):cs.system.data.DataSet;
	/**
	 * Gets or sets a value that is used for code persistence.
	 * @return A value that is used for code persistence.
	 */
	var DataViewSettingCollectionString(default, default):String;
	/**
	 * Gets the  for each  in the .
	 * @return A  for each .
	 */
	var DataViewSettings(default, never):cs.system.data.DataViewSettingCollection;
	@:overload(function():Void {})
	function new(dataSet:cs.system.data.DataSet):Void;
	/**
	 * Creates a  for the specified .
	 * @param table The name of the  to use in the .
	 * @return A  object.
	 */
	function CreateDataView(table:cs.system.data.DataTable):cs.system.data.DataView;
}
