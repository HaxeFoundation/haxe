package cs.system.data.common;

/** Represents a column within a data source. */
@:native("System.Data.Common.DbColumn")
extern class DbColumn {
	/**
	 * Gets a nullable boolean value that indicates whether  values are allowed in this
	 * column, or returns  if no value is set. Can be set to either  or  indicating
	 * whether  values are allowed in this column, or  ( in Visual Basic) when
	 * overridden in a derived class.
	 * @return Returns  if  values are allowed in this column; otherwise, . If no value
	 * is set, returns a null reference ( in Visual Basic).
	 */
	var AllowDBNull(default, default):Null<Bool>;
	/**
	 * Gets the catalog name associated with the data source; otherwise,  if no value
	 * is set. Can be set to either the catalog name or  when overridden in a derived
	 * class.
	 * @return The catalog name associated with the data source; otherwise, a null
	 * reference ( in Visual Basic) if no value is set.
	 */
	var BaseCatalogName(default, default):String;
	/**
	 * Gets the base column name; otherwise,  if no value is set. Can be set to either
	 * the column name or  when overridden in a derived class.
	 * @return The base column name; otherwise, a null reference ( in Visual Basic) if
	 * no value is set.
	 */
	var BaseColumnName(default, default):String;
	/**
	 * Gets the schema name associated with the data source; otherwise,  if no value is
	 * set. Can be set to either the schema name or  when overridden in a derived
	 * class.
	 * @return The schema name associated with the data source; otherwise, a null
	 * reference ( in Visual Basic) if no value is set.
	 */
	var BaseSchemaName(default, default):String;
	/**
	 * Gets the server name associated with the column; otherwise,  if no value is set.
	 * Can be set to either the server name or  when overridden in a derived class.
	 * @return The server name associated with the column; otherwise, a null reference
	 * ( in Visual Basic) if no value is set.
	 */
	var BaseServerName(default, default):String;
	/**
	 * Gets the table name in the schema; otherwise,  if no value is set. Can be set to
	 * either the table name or  when overridden in a derived class.
	 * @return The table name in the schema; otherwise, a null reference ( in Visual
	 * Basic) if no value is set.
	 */
	var BaseTableName(default, default):String;
	/**
	 * Gets the name of the column. Can be set to the column name when overridden in a
	 * derived class.
	 * @return The name of the column.
	 */
	var ColumnName(default, default):String;
	/**
	 * Gets the column position (ordinal) in the datasource row; otherwise,  if no
	 * value is set. Can be set to either an  value to specify the column position or 
	 * when overridden in a derived class.
	 * @return An  value for column ordinal; otherwise, a null reference ( in Visual
	 * Basic) if no value is set.
	 */
	var ColumnOrdinal(default, default):Null<Int>;
	/**
	 * Gets the column size; otherwise,  if no value is set. Can be set to either an 
	 * value to specify the column size or  when overridden in a derived class.
	 * @return An  value for column size; otherwise, a null reference ( in Visual
	 * Basic) if no value is set.
	 */
	var ColumnSize(default, default):Null<Int>;
	/**
	 * Gets the type of data stored in the column. Can be set to a  object that
	 * represents the type of data in the column when overridden in a derived class.
	 * @return A  object that represents the type of data the column contains.
	 */
	var DataType(default, default):cs.system.Type;
	/**
	 * Gets the name of the data type; otherwise,  if no value is set. Can be set to
	 * either the data type name or  when overridden in a derived class.
	 * @return The name of the data type; otherwise, a null reference ( in Visual
	 * Basic) if no value is set.
	 */
	var DataTypeName(default, default):String;
	/**
	 * Gets a nullable boolean value that indicates whether this column is aliased, or
	 * returns  if no value is set. Can be set to either  or  indicating whether this
	 * column is aliased, or  ( in Visual Basic) when overridden in a derived class.
	 * @return Returns  if this column is aliased; otherwise, . If no value is set,
	 * returns a null reference ( in Visual Basic).
	 */
	var IsAliased(default, default):Null<Bool>;
	/**
	 * Gets a nullable boolean value that indicates whether values in this column are
	 * automatically incremented, or returns  if no value is set. Can be set to either 
	 * or  indicating whether values in this column are automatically incremented, or 
	 * ( in Visual Basic) when overridden in a derived class.
	 * @return Returns  if values in this column are automatically incremented;
	 * otherwise, . If no value is set, returns a null reference ( in Visual Basic).
	 */
	var IsAutoIncrement(default, default):Null<Bool>;
	/**
	 * Gets a nullable boolean value that indicates whether this column is an
	 * expression, or returns  if no value is set. Can be set to either  or  indicating
	 * whether this column is an expression, or  ( in Visual Basic) when overridden in
	 * a derived class.
	 * @return Returns  if this column is an expression; otherwise, . If no value is
	 * set, returns a null reference ( in Visual Basic).
	 */
	var IsExpression(default, default):Null<Bool>;
	/**
	 * Gets a nullable boolean value that indicates whether this column is hidden, or
	 * returns  if no value is set. Can be set to either  or  indicating whether this
	 * column is hidden, or  ( in Visual Basic) when overridden in a derived class.
	 * @return Returns  if this column is hidden; otherwise, . If no value is set,
	 * returns a null reference ( in Visual Basic).
	 */
	var IsHidden(default, default):Null<Bool>;
	/**
	 * Gets a nullable boolean value that indicates whether this column is an identity,
	 * or returns  if no value is set. Can be set to either  or  indicating whether
	 * this column is an identity, or  ( in Visual Basic) when overridden in a derived
	 * class.
	 * @return Returns  if this column is an identity; otherwise, . If no value is set,
	 * returns a null reference ( in Visual Basic).
	 */
	var IsIdentity(default, default):Null<Bool>;
	/**
	 * Gets a nullable boolean value that indicates whether this column is a key, or
	 * returns  if no value is set. Can be set to either  or  indicating whether this
	 * column is a key, or  ( in Visual Basic) when overridden in a derived class.
	 * @return Returns  if this column is a key; otherwise, . If no value is set,
	 * returns a null reference ( in Visual Basic).
	 */
	var IsKey(default, default):Null<Bool>;
	/**
	 * Gets a nullable boolean value that indicates whether this column contains long
	 * data, or returns  if no value is set. Can be set to either  or  indicating
	 * whether this column contains long data, or  ( in Visual Basic) when overridden
	 * in a derived class.
	 * @return Returns  if this column contains long data; otherwise, . If no value is
	 * set, returns a null reference ( in Visual Basic).
	 */
	var IsLong(default, default):Null<Bool>;
	/**
	 * Gets a nullable boolean value that indicates whether this column is read-only,
	 * or returns  if no value is set. Can be set to either  or  indicating whether
	 * this column is read-only, or  ( in Visual Basic) when overridden in a derived
	 * class.
	 * @return Returns  if this column is read-only; otherwise, . If no value is set,
	 * returns a null reference ( in Visual Basic).
	 */
	var IsReadOnly(default, default):Null<Bool>;
	/**
	 * Gets a nullable boolean value that indicates whether a unique constraint applies
	 * to this column, or returns  if no value is set. Can be set to either  or 
	 * indicating whether a unique constraint applies to this column, or  ( in Visual
	 * Basic) when overridden in a derived class.
	 * @return Returns  if a unique constraint applies to this column; otherwise, . If
	 * no value is set, returns a null reference ( in Visual Basic).
	 */
	var IsUnique(default, default):Null<Bool>;
	/**
	 * Gets the numeric precision of the column data; otherwise,  if no value is set.
	 * Can be set to either an  value to specify the numeric precision of the column
	 * data or  when overridden in a derived class.
	 * @return An  value that specifies the precision of the column data, if the data
	 * is numeric; otherwise, a null reference ( in Visual Basic) if no value is set.
	 */
	var NumericPrecision(default, default):Null<Int>;
	/**
	 * Gets a nullable  value that either returns  or the numeric scale of the column
	 * data. Can be set to either  or an  value for the numeric scale of the column
	 * data when overridden in a derived class.
	 * @return A null reference ( in Visual Basic) if no value is set; otherwise, a 
	 * value that specifies the scale of the column data, if the data is numeric.
	 */
	var NumericScale(default, default):Null<Int>;
	/**
	 * Gets the assembly-qualified name of the  object that represents the type of data
	 * in the column; otherwise,  if no value is set. Can be set to either the
	 * assembly-qualified name or  when overridden in a derived class.
	 * @return The assembly-qualified name of the  object that represents the type of
	 * data in the column; otherwise, a null reference ( in Visual Basic) if no value
	 * is set.
	 */
	var UdtAssemblyQualifiedName(default, default):String;
	@:native("get_Item")
	function get_Item(index0:String):Dynamic;
}
