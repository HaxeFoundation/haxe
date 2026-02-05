package cs.system.data.common;

/** Describes the column metadata of the schema for a database table. */
@:native("System.Data.Common.SchemaTableColumn")
extern class SchemaTableColumn {
	/** Specifies whether value  is allowed. */
	static var AllowDBNull(default, never):String;
	/** Specifies the name of the column in the schema table. */
	static var BaseColumnName(default, never):String;
	/** Specifies the name of the schema in the schema table. */
	static var BaseSchemaName(default, never):String;
	/** Specifies the name of the table in the schema table. */
	static var BaseTableName(default, never):String;
	/** Specifies the name of the column in the schema table. */
	static var ColumnName(default, never):String;
	/** Specifies the ordinal of the column. */
	static var ColumnOrdinal(default, never):String;
	/** Specifies the size of the column. */
	static var ColumnSize(default, never):String;
	/** Specifies the type of data in the column. */
	static var DataType(default, never):String;
	/** Specifies whether this column is aliased. */
	static var IsAliased(default, never):String;
	/** Specifies whether this column is an expression. */
	static var IsExpression(default, never):String;
	/** Specifies whether this column is a key for the table. */
	static var IsKey(default, never):String;
	/** Specifies whether this column contains long data. */
	static var IsLong(default, never):String;
	/** Specifies whether a unique constraint applies to this column. */
	static var IsUnique(default, never):String;
	/** Specifies the non-versioned provider-specific data type of the column. */
	static var NonVersionedProviderType(default, never):String;
	/** Specifies the precision of the column data, if the data is numeric. */
	static var NumericPrecision(default, never):String;
	/** Specifies the scale of the column data, if the data is numeric. */
	static var NumericScale(default, never):String;
	/** Specifies the provider-specific data type of the column. */
	static var ProviderType(default, never):String;
}
