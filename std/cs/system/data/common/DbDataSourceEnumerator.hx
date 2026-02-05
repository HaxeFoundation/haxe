package cs.system.data.common;

/** Provides a mechanism for enumerating all available instances of database servers within the local network. */
@:native("System.Data.Common.DbDataSourceEnumerator")
extern class DbDataSourceEnumerator {
	/**
	 * Retrieves a  containing information about all visible instances of the server
	 * represented by the strongly typed instance of this class.
	 * @return A  containing information about the visible instances of the associated
	 * data source.
	 */
	function GetDataSources():cs.system.data.DataTable;
}
