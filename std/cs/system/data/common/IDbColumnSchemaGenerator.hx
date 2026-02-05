package cs.system.data.common;

/** Generates a column schema. */
@:native("System.Data.Common.IDbColumnSchemaGenerator")
extern interface IDbColumnSchemaGenerator {
	/**
	 * Gets the column schema ( collection).
	 * @return The column schema ( collection).
	 */
	function GetColumnSchema():cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.data.common.DbColumn>;
}
