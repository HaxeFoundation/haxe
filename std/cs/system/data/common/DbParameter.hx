package cs.system.data.common;

/** Represents a parameter to a  and optionally, its mapping to a  column. For more information on parameters, see Configuring Parameters and Parameter Data Types. */
@:native("System.Data.Common.DbParameter")
extern class DbParameter extends cs.system.MarshalByRefObject {
	/**
	 * Gets or sets the  of the parameter.
	 * @return One of the  values. The default is .
	 */
	var DbType(default, default):cs.system.data.DbType;
	/**
	 * Gets or sets a value that indicates whether the parameter is input-only,
	 * output-only, bidirectional, or a stored procedure return value parameter.
	 * @return One of the  values. The default is .
	 */
	var Direction(default, default):cs.system.data.ParameterDirection;
	/**
	 * Gets or sets a value that indicates whether the parameter accepts null values.
	 * @return if null values are accepted; otherwise . The default is .
	 */
	var IsNullable(default, default):Bool;
	/**
	 * Gets or sets the name of the .
	 * @return The name of the . The default is an empty string ("").
	 */
	var ParameterName(default, default):String;
	/**
	 * Gets or sets the maximum number of digits used to represent the  property.
	 * @return The maximum number of digits used to represent the  property.
	 */
	var Precision(default, default):cs.UInt8;
	/**
	 * Gets or sets the number of decimal places to which  is resolved.
	 * @return The number of decimal places to which  is resolved.
	 */
	var Scale(default, default):cs.UInt8;
	/**
	 * Gets or sets the maximum size, in bytes, of the data within the column.
	 * @return The maximum size, in bytes, of the data within the column. The default
	 * value is inferred from the parameter value.
	 */
	var Size(default, default):Int;
	/**
	 * Gets or sets the name of the source column mapped to the  and used for loading
	 * or returning the .
	 * @return The name of the source column mapped to the . The default is an empty
	 * string.
	 */
	var SourceColumn(default, default):String;
	/**
	 * Sets or gets a value which indicates whether the source column is nullable. This
	 * allows  to correctly generate Update statements for nullable columns.
	 * @return if the source column is nullable;  if it is not.
	 */
	var SourceColumnNullMapping(default, default):Bool;
	/**
	 * Gets or sets the  to use when you load .
	 * @return One of the  values. The default is .
	 */
	var SourceVersion(default, default):cs.system.data.DataRowVersion;
	/**
	 * Gets or sets the value of the parameter.
	 * @return An  that is the value of the parameter. The default value is null.
	 */
	var Value(default, default):Dynamic;
	/** Resets the DbType property to its original settings. */
	function ResetDbType():Void;
}
