package cs.system.data.common;

/** Represents a set of methods for creating instances of a provider's implementation of the data source classes. */
@:native("System.Data.Common.DbProviderFactory")
extern class DbProviderFactory {
	var CanCreateCommandBuilder(default, never):Bool;
	var CanCreateDataAdapter(default, never):Bool;
	/**
	 * Specifies whether the specific  supports the  class.
	 * @return if the instance of the  supports the  class; otherwise .
	 */
	var CanCreateDataSourceEnumerator(default, never):Bool;
	/**
	 * Returns a new instance of the provider's class that implements the  class.
	 * @return A new instance of .
	 */
	function CreateCommand():cs.system.data.common.DbCommand;
	/**
	 * Returns a new instance of the provider's class that implements the  class.
	 * @return A new instance of .
	 */
	function CreateCommandBuilder():cs.system.data.common.DbCommandBuilder;
	/**
	 * Returns a new instance of the provider's class that implements the  class.
	 * @return A new instance of .
	 */
	function CreateConnection():cs.system.data.common.DbConnection;
	/**
	 * Returns a new instance of the provider's class that implements the  class.
	 * @return A new instance of .
	 */
	function CreateConnectionStringBuilder():cs.system.data.common.DbConnectionStringBuilder;
	/**
	 * Returns a new instance of the provider's class that implements the  class.
	 * @return A new instance of .
	 */
	function CreateDataAdapter():cs.system.data.common.DbDataAdapter;
	/**
	 * Returns a new instance of the provider's class that implements the  class.
	 * @return A new instance of .
	 */
	function CreateDataSourceEnumerator():cs.system.data.common.DbDataSourceEnumerator;
	/**
	 * Returns a new instance of the provider's class that implements the  class.
	 * @return A new instance of .
	 */
	function CreateParameter():cs.system.data.common.DbParameter;
}
