package cs.system.data;

/** The  obtains the contents of one or more  objects in the form of one or more read-only, forward-only result sets. */
@:native("System.Data.DataTableReader")
extern class DataTableReader extends cs.system.data.common.DbDataReader {
	@:overload(function(dataTable:cs.system.data.DataTable):Void {})
	function new(dataTables:cs.NativeArray<cs.system.data.DataTable>):Void;
	/** Closes the current . */
	function Close():Void;
	/**
	 * Gets the value of the specified column as a .
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetBoolean(ordinal:Int):Bool;
	/**
	 * Gets the value of the specified column as a byte.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetByte(ordinal:Int):cs.UInt8;
	/**
	 * Reads a stream of bytes starting at the specified column offset into the buffer
	 * as an array starting at the specified buffer offset.
	 * @param ordinal The zero-based column ordinal.
	 * @param dataIndex The index within the field from which to start the read
	 * operation.
	 * @param buffer The buffer into which to read the stream of bytes.
	 * @param bufferIndex The index within the buffer at which to start placing the
	 * data.
	 * @param length The maximum length to copy into the buffer.
	 * @return The actual number of bytes read.
	 */
	function GetBytes(ordinal:Int, dataIndex:haxe.Int64, buffer:cs.NativeArray<cs.UInt8>, bufferIndex:Int, length:Int):haxe.Int64;
	/**
	 * Gets the value of the specified column as a character.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the column.
	 */
	function GetChar(ordinal:Int):cs.Char16;
	/**
	 * Returns the value of the specified column as a character array.
	 * @param ordinal The zero-based column ordinal.
	 * @param dataIndex The index within the field from which to start the read
	 * operation.
	 * @param buffer The buffer into which to read the stream of chars.
	 * @param bufferIndex The index within the buffer at which to start placing the
	 * data.
	 * @param length The maximum length to copy into the buffer.
	 * @return The actual number of characters read.
	 */
	function GetChars(ordinal:Int, dataIndex:haxe.Int64, buffer:cs.NativeArray<cs.Char16>, bufferIndex:Int, length:Int):haxe.Int64;
	/**
	 * Gets a string representing the data type of the specified column.
	 * @param ordinal The zero-based column ordinal.
	 * @return A string representing the column's data type.
	 */
	function GetDataTypeName(ordinal:Int):String;
	/**
	 * Gets the value of the specified column as a  object.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetDateTime(ordinal:Int):cs.system.DateTime;
	/**
	 * Gets the value of the specified column as a .
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetDecimal(ordinal:Int):cs.system.Decimal;
	/**
	 * Gets the value of the column as a double-precision floating point number.
	 * @param ordinal The zero-based ordinal of the column.
	 * @return The value of the specified column.
	 */
	function GetDouble(ordinal:Int):Float;
	/**
	 * Returns an enumerator that can be used to iterate through the item collection.
	 * @return An  object that represents the item collection.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Gets the  that is the data type of the object.
	 * @param ordinal The zero-based column ordinal.
	 * @return The  that is the data type of the object.
	 */
	function GetFieldType(ordinal:Int):cs.system.Type;
	/**
	 * Gets the value of the specified column as a single-precision floating point
	 * number.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the column.
	 */
	function GetFloat(ordinal:Int):Single;
	/**
	 * Gets the value of the specified column as a globally-unique identifier (GUID).
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetGuid(ordinal:Int):cs.system.Guid;
	/**
	 * Gets the value of the specified column as a 16-bit signed integer.
	 * @param ordinal The zero-based column ordinal
	 * @return The value of the specified column.
	 */
	function GetInt16(ordinal:Int):cs.Int16;
	/**
	 * Gets the value of the specified column as a 32-bit signed integer.
	 * @param ordinal The zero-based column ordinal
	 * @return The value of the specified column.
	 */
	function GetInt32(ordinal:Int):Int;
	/**
	 * Gets the value of the specified column as a 64-bit signed integer.
	 * @param ordinal The zero-based column ordinal
	 * @return The value of the specified column.
	 */
	function GetInt64(ordinal:Int):haxe.Int64;
	/**
	 * Gets the value of the specified column as a .
	 * @param ordinal The zero-based column ordinal
	 * @return The name of the specified column.
	 */
	function GetName(ordinal:Int):String;
	/**
	 * Gets the column ordinal, given the name of the column.
	 * @param name The name of the column.
	 * @return The zero-based column ordinal.
	 */
	function GetOrdinal(name:String):Int;
	/**
	 * Gets the type of the specified column in provider-specific format.
	 * @param ordinal The zero-based column ordinal.
	 * @return The  that is the data type of the object.
	 */
	function GetProviderSpecificFieldType(ordinal:Int):cs.system.Type;
	/**
	 * Gets the value of the specified column in provider-specific format.
	 * @param ordinal The zero-based number of the column whose value is retrieved.
	 * @return The value of the specified column in provider-specific format.
	 */
	function GetProviderSpecificValue(ordinal:Int):Dynamic;
	/**
	 * Fills the supplied array with provider-specific type information for all the
	 * columns in the .
	 * @param values An array of objects to be filled in with type information for the
	 * columns in the .
	 * @return The number of column values copied into the array.
	 */
	function GetProviderSpecificValues(values:cs.NativeArray<Dynamic>):Int;
	/**
	 * Returns a  that describes the column metadata of the .
	 * @return A  that describes the column metadata.
	 */
	function GetSchemaTable():cs.system.data.DataTable;
	/**
	 * Gets the value of the specified column as a string.
	 * @param ordinal The zero-based column ordinal
	 * @return The value of the specified column.
	 */
	function GetString(ordinal:Int):String;
	/**
	 * Gets the value of the specified column in its native format.
	 * @param ordinal The zero-based column ordinal
	 * @return The value of the specified column. This method returns  for null
	 * columns.
	 */
	function GetValue(ordinal:Int):Dynamic;
	/**
	 * Populates an array of objects with the column values of the current row.
	 * @param values An array of  into which to copy the column values from the .
	 * @return The number of column values copied into the array.
	 */
	function GetValues(values:cs.NativeArray<Dynamic>):Int;
	/**
	 * Gets a value that indicates whether the column contains non-existent or missing
	 * values.
	 * @param ordinal The zero-based column ordinal
	 * @return if the specified column value is equivalent to ; otherwise, .
	 */
	function IsDBNull(ordinal:Int):Bool;
	/**
	 * Advances the  to the next result set, if any.
	 * @return if there was another result set; otherwise .
	 */
	function NextResult():Bool;
	/**
	 * Advances the  to the next record.
	 * @return if there was another row to read; otherwise .
	 */
	function Read():Bool;
}
