package cs.system.data.common;

/** Describes optional column metadata of the schema for a database table. */
@:native("System.Data.Common.SchemaTableOptionalColumn")
extern class SchemaTableOptionalColumn {
	/** Specifies the value at which the series for new identity columns is assigned. */
	static var AutoIncrementSeed(default, never):String;
	/** Specifies the increment between values in the identity column. */
	static var AutoIncrementStep(default, never):String;
	/** The name of the catalog associated with the results of the latest query. */
	static var BaseCatalogName(default, never):String;
	/** The namespace of the column. */
	static var BaseColumnNamespace(default, never):String;
	/** The server name of the column. */
	static var BaseServerName(default, never):String;
	/** The namespace for the table that contains the column. */
	static var BaseTableNamespace(default, never):String;
	/** Specifies the mapping for the column. */
	static var ColumnMapping(default, never):String;
	/** The default value for the column. */
	static var DefaultValue(default, never):String;
	/** The expression used to compute the column. */
	static var Expression(default, never):String;
	/** Specifies whether the column values in the column are automatically incremented. */
	static var IsAutoIncrement(default, never):String;
	/** Specifies whether this column is hidden. */
	static var IsHidden(default, never):String;
	/** Specifies whether this column is read-only. */
	static var IsReadOnly(default, never):String;
	/** Specifies whether this column contains row version information. */
	static var IsRowVersion(default, never):String;
	/** Specifies the provider-specific data type of the column. */
	static var ProviderSpecificDataType(default, never):String;
}
