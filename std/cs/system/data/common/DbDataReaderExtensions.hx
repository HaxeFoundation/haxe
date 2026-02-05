package cs.system.data.common;

/** This class contains column schema extension methods for . */
@:native("System.Data.Common.DbDataReaderExtensions")
extern class DbDataReaderExtensions {
	/**
	 * Gets a value that indicates whether a  can get a column schema.
	 * @param reader The  to be checked for column schema support.
	 * @return if the  can get a column schema; otherwise, .
	 */
	static function CanGetColumnSchema(reader:cs.system.data.common.DbDataReader):Bool;
	/**
	 * Gets the column schema ( collection) for a .
	 * @param reader The  to return the column schema.
	 * @return The column schema ( collection) for a .
	 */
	static function GetColumnSchema(reader:cs.system.data.common.DbDataReader):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.data.common.DbColumn>;
}
