package cs.system.data;

/** Associates a data source column with a  column, and is implemented by the  class, which is used in common by .NET Framework data providers. */
@:native("System.Data.IColumnMapping")
extern interface IColumnMapping {
	/**
	 * Gets or sets the name of the column within the  to map to.
	 * @return The name of the column within the  to map to. The name is not case
	 * sensitive.
	 */
	var DataSetColumn(default, default):String;
	/**
	 * Gets or sets the name of the column within the data source to map from. The name
	 * is case-sensitive.
	 * @return The case-sensitive name of the column in the data source.
	 */
	var SourceColumn(default, default):String;
}
