package cs.system.data;

/** Associates a source table with a table in a , and is implemented by the  class, which is used in common by .NET Framework data providers. */
@:native("System.Data.ITableMapping")
extern interface ITableMapping {
	/**
	 * Gets the derived  for the .
	 * @return A collection of data column mappings.
	 */
	var ColumnMappings(default, never):cs.system.data.IColumnMappingCollection;
	/**
	 * Gets or sets the case-insensitive name of the table within the .
	 * @return The case-insensitive name of the table within the .
	 */
	var DataSetTable(default, default):String;
	/**
	 * Gets or sets the case-sensitive name of the source table.
	 * @return The case-sensitive name of the source table.
	 */
	var SourceTable(default, default):String;
}
