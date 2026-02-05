package cs.system.data;

/** Represents the schema of a column in a . */
@:native("System.Data.DataColumn")
extern class DataColumn extends cs.system.componentmodel.MarshalByValueComponent {
	/**
	 * Gets or sets a value that indicates whether null values are allowed in this
	 * column for rows that belong to the table.
	 * @return if null values are allowed; otherwise, . The default is .
	 */
	var AllowDBNull(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether the column automatically increments
	 * the value of the column for new rows added to the table.
	 * @return if the value of the column increments automatically; otherwise, . The
	 * default is .
	 */
	var AutoIncrement(default, default):Bool;
	/**
	 * Gets or sets the starting value for a column that has its  property set to . The
	 * default is 0.
	 * @return The starting value for the  feature.
	 */
	var AutoIncrementSeed(default, default):haxe.Int64;
	/**
	 * Gets or sets the increment used by a column with its  property set to .
	 * @return The number by which the value of the column is automatically
	 * incremented. The default is 1.
	 */
	var AutoIncrementStep(default, default):haxe.Int64;
	/**
	 * Gets or sets the caption for the column.
	 * @return The caption of the column. If not set, returns the  value.
	 */
	var Caption(default, default):String;
	/**
	 * Gets or sets the  of the column.
	 * @return One of the  values.
	 */
	var ColumnMapping(default, default):cs.system.data.MappingType;
	/**
	 * Gets or sets the name of the column in the .
	 * @return The name of the column.
	 */
	var ColumnName(default, default):String;
	/**
	 * Gets or sets the type of data stored in the column.
	 * @return A  object that represents the column data type.
	 */
	var DataType(default, default):cs.system.Type;
	/**
	 * Gets or sets the  for the column.
	 * @return The  for the specified column.
	 */
	var DateTimeMode(default, default):cs.system.data.DataSetDateTime;
	/**
	 * Gets or sets the default value for the column when you are creating new rows.
	 * @return A value appropriate to the column's .
	 */
	var DefaultValue(default, default):Dynamic;
	/**
	 * Gets or sets the expression used to filter rows, calculate the values in a
	 * column, or create an aggregate column.
	 * @return An expression to calculate the value of a column, or create an aggregate
	 * column. The return type of an expression is determined by the  of the column.
	 */
	var Expression(default, default):String;
	/**
	 * Gets the collection of custom user information associated with a .
	 * @return A  of custom information.
	 */
	var ExtendedProperties(default, never):cs.system.data.PropertyCollection;
	/**
	 * Gets or sets the maximum length of a text column.
	 * @return The maximum length of the column in characters. If the column has no
	 * maximum length, the value is -1 (default).
	 */
	var MaxLength(default, default):Int;
	/**
	 * Gets or sets the namespace of the .
	 * @return The namespace of the .
	 */
	var Namespace(default, default):String;
	/**
	 * Gets the (zero-based) position of the column in the  collection.
	 * @return The position of the column. Gets -1 if the column is not a member of a
	 * collection.
	 */
	var Ordinal(default, never):Int;
	/**
	 * Gets or sets an XML prefix that aliases the namespace of the .
	 * @return The XML prefix for the  namespace.
	 */
	var Prefix(default, default):String;
	/**
	 * Gets or sets a value that indicates whether the column allows for changes as
	 * soon as a row has been added to the table.
	 * @return if the column is read only; otherwise, . The default is .
	 */
	var ReadOnly(default, default):Bool;
	/**
	 * Gets the  to which the column belongs to.
	 * @return The  that the  belongs to.
	 */
	var Table(default, never):cs.system.data.DataTable;
	/**
	 * Gets or sets a value that indicates whether the values in each row of the column
	 * must be unique.
	 * @return if the value must be unique; otherwise, . The default is .
	 */
	var Unique(default, default):Bool;
	@:overload(function():Void {})
	@:overload(function(columnName:String):Void {})
	@:overload(function(columnName:String, dataType:cs.system.Type):Void {})
	@:overload(function(columnName:String, dataType:cs.system.Type, expr:String):Void {})
	function new(columnName:String, dataType:cs.system.Type, expr:String, type:cs.system.data.MappingType):Void;
	/**
	 * Changes the ordinal or position of the  to the specified ordinal or position.
	 * @param ordinal The specified ordinal.
	 */
	function SetOrdinal(ordinal:Int):Void;
	/**
	 * Gets the  of the column, if one exists.
	 * @return The  value, if the property is set; otherwise, the  property.
	 */
	function ToString():String;
}
