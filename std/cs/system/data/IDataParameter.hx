package cs.system.data;

/** Represents a parameter to a Command object, and optionally, its mapping to  columns; and is implemented by .NET Framework data providers that access data sources. */
@:native("System.Data.IDataParameter")
extern interface IDataParameter {
	/**
	 * Gets or sets the  of the parameter.
	 * @return One of the  values. The default is .
	 */
	var DbType(default, default):cs.system.data.DbType;
	/**
	 * Gets or sets a value indicating whether the parameter is input-only,
	 * output-only, bidirectional, or a stored procedure return value parameter.
	 * @return One of the  values. The default is .
	 */
	var Direction(default, default):cs.system.data.ParameterDirection;
	/**
	 * Gets a value indicating whether the parameter accepts null values.
	 * @return if null values are accepted; otherwise, . The default is .
	 */
	var IsNullable(default, never):Bool;
	/**
	 * Gets or sets the name of the .
	 * @return The name of the . The default is an empty string.
	 */
	var ParameterName(default, default):String;
	/**
	 * Gets or sets the name of the source column that is mapped to the  and used for
	 * loading or returning the .
	 * @return The name of the source column that is mapped to the . The default is an
	 * empty string.
	 */
	var SourceColumn(default, default):String;
	/**
	 * Gets or sets the  to use when loading .
	 * @return One of the  values. The default is .
	 */
	var SourceVersion(default, default):cs.system.data.DataRowVersion;
	/**
	 * Gets or sets the value of the parameter.
	 * @return An  that is the value of the parameter. The default value is null.
	 */
	var Value(default, default):Dynamic;
}
