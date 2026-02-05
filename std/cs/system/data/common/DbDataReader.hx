package cs.system.data.common;

/** Reads a forward-only stream of rows from a data source. */
@:native("System.Data.Common.DbDataReader")
extern class DbDataReader extends cs.system.MarshalByRefObject {
	/**
	 * Gets a value indicating the depth of nesting for the current row.
	 * @return The depth of nesting for the current row.
	 */
	var Depth(default, never):Int;
	/**
	 * Gets the number of columns in the current row.
	 * @return The number of columns in the current row.
	 */
	var FieldCount(default, never):Int;
	/**
	 * Gets a value that indicates whether this  contains one or more rows.
	 * @return if the  contains one or more rows; otherwise, .
	 */
	var HasRows(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is closed.
	 * @return if the  is closed; otherwise, .
	 */
	var IsClosed(default, never):Bool;
	/**
	 * Gets the number of rows changed, inserted, or deleted by execution of the SQL
	 * statement.
	 * @return The number of rows changed, inserted, or deleted. -1 for SELECT
	 * statements; 0 if no rows were affected or the statement failed.
	 */
	var RecordsAffected(default, never):Int;
	/**
	 * Gets the number of fields in the  that are not hidden.
	 * @return The number of fields that are not hidden.
	 */
	var VisibleFieldCount(default, never):Int;
	@:overload(function(index0:Int):Dynamic {})
	@:native("get_Item")
	function get_Item(index0:String):Dynamic;
	/** Closes the  object. */
	function Close():Void;
	function CloseAsync():cs.system.threading.tasks.Task;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Gets the value of the specified column as a Boolean.
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
	 * Reads a specified number of bytes from the specified column starting at a
	 * specified index and writes them to a buffer starting at a specified position in
	 * the buffer.
	 * @param ordinal The zero-based column ordinal.
	 * @param dataOffset The index within the row from which to begin the read
	 * operation.
	 * @param buffer The buffer into which to copy the data.
	 * @param bufferOffset The index with the buffer to which the data will be copied.
	 * @param length The maximum number of characters to read.
	 * @return The actual number of bytes read.
	 */
	function GetBytes(ordinal:Int, dataOffset:haxe.Int64, buffer:cs.NativeArray<cs.UInt8>, bufferOffset:Int, length:Int):haxe.Int64;
	/**
	 * Gets the value of the specified column as a single character.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetChar(ordinal:Int):cs.Char16;
	/**
	 * Reads a specified number of characters from a specified column starting at a
	 * specified index, and writes them to a buffer starting at a specified position.
	 * @param ordinal The zero-based column ordinal.
	 * @param dataOffset The index within the row from which to begin the read
	 * operation.
	 * @param buffer The buffer into which to copy the data.
	 * @param bufferOffset The index with the buffer to which the data will be copied.
	 * @param length The maximum number of characters to read.
	 * @return The actual number of characters read.
	 */
	function GetChars(ordinal:Int, dataOffset:haxe.Int64, buffer:cs.NativeArray<cs.Char16>, bufferOffset:Int, length:Int):haxe.Int64;
	/**
	 * Returns a nested data reader for the requested column.
	 * @param ordinal The zero-based column ordinal.
	 * @return A data reader.
	 */
	function GetData(ordinal:Int):cs.system.data.common.DbDataReader;
	/**
	 * Gets name of the data type of the specified column.
	 * @param ordinal The zero-based column ordinal.
	 * @return The name of the data type.
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
	 * Gets the value of the specified column as a double-precision floating point
	 * number.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetDouble(ordinal:Int):Float;
	/**
	 * Returns an  that can be used to iterate through the rows in the data reader.
	 * @return An  that can be used to iterate through the rows in the data reader.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Gets the data type of the specified column.
	 * @param ordinal The zero-based column ordinal.
	 * @return The data type of the specified column.
	 */
	function GetFieldType(ordinal:Int):cs.system.Type;
	/**
	 * Gets the value of the specified column as the requested type.
	 * @param T The type of the value to be returned.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetFieldValue<T>(ordinal:Int):T;
	@:overload(function<T>(ordinal:Int):cs.system.threading.tasks.Task_1<T> {})
	/**
	 * Asynchronously gets the value of the specified column as the requested type.
	 * @param T The type of the value to be returned.
	 * @param ordinal The zero-based column ordinal.
	 * @return A task whose  contains the value of the specified column.
	 */
	function GetFieldValueAsync<T>(ordinal:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<T>;
	/**
	 * Gets the value of the specified column as a single-precision floating point
	 * number.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetFloat(ordinal:Int):Single;
	/**
	 * Gets the value of the specified column as a globally unique identifier (GUID).
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetGuid(ordinal:Int):cs.system.Guid;
	/**
	 * Gets the value of the specified column as a 16-bit signed integer.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetInt16(ordinal:Int):cs.Int16;
	/**
	 * Gets the value of the specified column as a 32-bit signed integer.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetInt32(ordinal:Int):Int;
	/**
	 * Gets the value of the specified column as a 64-bit signed integer.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetInt64(ordinal:Int):haxe.Int64;
	/**
	 * Gets the name of the column, given the zero-based column ordinal.
	 * @param ordinal The zero-based column ordinal.
	 * @return The name of the specified column.
	 */
	function GetName(ordinal:Int):String;
	/**
	 * Gets the column ordinal given the name of the column.
	 * @param name The name of the column.
	 * @return The zero-based column ordinal.
	 */
	function GetOrdinal(name:String):Int;
	/**
	 * Gets the provider-specific type of the specified column.
	 * @param ordinal The zero-based column ordinal.
	 * @return A provider-specific .NET type.
	 */
	function GetProviderSpecificFieldType(ordinal:Int):cs.system.Type;
	/**
	 * Gets the value of the specified column as an instance of a provider-specific
	 * type.
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetProviderSpecificValue(ordinal:Int):Dynamic;
	/**
	 * Gets all provider-specific attribute columns in the collection for the current
	 * row.
	 * @param values An array of  into which to copy the attribute columns.
	 * @return The number of instances of elements in the array.
	 */
	function GetProviderSpecificValues(values:cs.NativeArray<Dynamic>):Int;
	/**
	 * Returns a  that describes the column metadata of the .
	 * @return A  that describes the column metadata.
	 */
	function GetSchemaTable():cs.system.data.DataTable;
	/**
	 * Gets a stream to retrieve data from the specified column.
	 * @param ordinal The zero-based column ordinal.
	 * @return A stream.
	 */
	function GetStream(ordinal:Int):cs.system.io.Stream;
	/**
	 * Gets the value of the specified column as an instance of .
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetString(ordinal:Int):String;
	/**
	 * Gets a text reader to retrieve data from the column.
	 * @param ordinal The zero-based column ordinal.
	 * @return A text reader.
	 */
	function GetTextReader(ordinal:Int):cs.system.io.TextReader;
	/**
	 * Gets the value of the specified column as an instance of .
	 * @param ordinal The zero-based column ordinal.
	 * @return The value of the specified column.
	 */
	function GetValue(ordinal:Int):Dynamic;
	/**
	 * Populates an array of objects with the column values of the current row.
	 * @param values An array of  into which to copy the attribute columns.
	 * @return The number of instances of  in the array.
	 */
	function GetValues(values:cs.NativeArray<Dynamic>):Int;
	/**
	 * Gets a value that indicates whether the column contains nonexistent or missing
	 * values.
	 * @param ordinal The zero-based column ordinal.
	 * @return if the specified column is equivalent to ; otherwise, .
	 */
	function IsDBNull(ordinal:Int):Bool;
	@:overload(function(ordinal:Int):cs.system.threading.tasks.Task_1<Bool> {})
	/**
	 * Asynchronously gets a value that indicates whether the column contains
	 * non-existent or missing values.
	 * @param ordinal The zero-based column to be retrieved.
	 * @return A  whose  property is  if the specified column value is equivalent to 
	 * or  if it is not.
	 */
	function IsDBNullAsync(ordinal:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Bool>;
	/**
	 * Advances the reader to the next result when reading the results of a batch of
	 * statements.
	 * @return if there are more result sets; otherwise, .
	 */
	function NextResult():Bool;
	@:overload(function():cs.system.threading.tasks.Task_1<Bool> {})
	/**
	 * Asynchronously advances the reader to the next result when reading the results
	 * of a batch of statements.
	 * @return A  whose  property is  if there are more result sets or  if there
	 * aren't.
	 */
	function NextResultAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Bool>;
	/**
	 * Advances the reader to the next record in a result set.
	 * @return if there are more rows; otherwise, .
	 */
	function Read():Bool;
	@:overload(function():cs.system.threading.tasks.Task_1<Bool> {})
	/**
	 * Asynchronously advances the reader to the next record in a result set.
	 * @return A  whose  property is  if there are more rows or  if there aren't.
	 */
	function ReadAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Bool>;
}
