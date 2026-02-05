package cs.system.data;

/** Used by the Visual Basic .NET Data Designers to represent a parameter to a Command object, and optionally, its mapping to  columns. */
@:native("System.Data.IDbDataParameter")
extern interface IDbDataParameter extends cs.system.data.IDataParameter {
	/**
	 * Indicates the precision of numeric parameters.
	 * @return The maximum number of digits used to represent the Value property of a
	 * data provider Parameter object. The default value is 0, which indicates that a
	 * data provider sets the precision for Value.
	 */
	var Precision(default, default):cs.UInt8;
	/**
	 * Indicates the scale of numeric parameters.
	 * @return The number of decimal places to which  is resolved. The default is 0.
	 */
	var Scale(default, default):cs.UInt8;
	/**
	 * The size of the parameter.
	 * @return The maximum size, in bytes, of the data within the column. The default
	 * value is inferred from the parameter value.
	 */
	var Size(default, default):Int;
}
