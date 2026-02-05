package cs.system.data;

/** Provides access to the column values within each row for a , and is implemented by .NET Framework data providers that access relational databases. */
@:native("System.Data.IDataRecord")
extern interface IDataRecord {
	/**
	 * Gets the number of columns in the current row.
	 * @return When not positioned in a valid recordset, 0; otherwise, the number of
	 * columns in the current record. The default is -1.
	 */
	var FieldCount(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):Dynamic;
	@:native("get_Item")
	function get_Item(index0:String):Dynamic;
	/**
	 * Gets the value of the specified column as a Boolean.
	 * @param i The zero-based column ordinal.
	 * @return The value of the column.
	 */
	function GetBoolean(i:Int):Bool;
	/**
	 * Gets the 8-bit unsigned integer value of the specified column.
	 * @param i The zero-based column ordinal.
	 * @return The 8-bit unsigned integer value of the specified column.
	 */
	function GetByte(i:Int):cs.UInt8;
	/**
	 * Reads a stream of bytes from the specified column offset into the buffer as an
	 * array, starting at the given buffer offset.
	 * @param i The zero-based column ordinal.
	 * @param fieldOffset The index within the field from which to start the read
	 * operation.
	 * @param buffer The buffer into which to read the stream of bytes.
	 * @param bufferoffset The index for  to start the read operation.
	 * @param length The number of bytes to read.
	 * @return The actual number of bytes read.
	 */
	function GetBytes(i:Int, fieldOffset:haxe.Int64, buffer:cs.NativeArray<cs.UInt8>, bufferoffset:Int, length:Int):haxe.Int64;
	/**
	 * Gets the character value of the specified column.
	 * @param i The zero-based column ordinal.
	 * @return The character value of the specified column.
	 */
	function GetChar(i:Int):cs.Char16;
	/**
	 * Reads a stream of characters from the specified column offset into the buffer as
	 * an array, starting at the given buffer offset.
	 * @param i The zero-based column ordinal.
	 * @param fieldoffset The index within the row from which to start the read
	 * operation.
	 * @param buffer The buffer into which to read the stream of bytes.
	 * @param bufferoffset The index for  to start the read operation.
	 * @param length The number of bytes to read.
	 * @return The actual number of characters read.
	 */
	function GetChars(i:Int, fieldoffset:haxe.Int64, buffer:cs.NativeArray<cs.Char16>, bufferoffset:Int, length:Int):haxe.Int64;
	/**
	 * Returns an  for the specified column ordinal.
	 * @param i The index of the field to find.
	 * @return The  for the specified column ordinal.
	 */
	function GetData(i:Int):cs.system.data.IDataReader;
	/**
	 * Gets the data type information for the specified field.
	 * @param i The index of the field to find.
	 * @return The data type information for the specified field.
	 */
	function GetDataTypeName(i:Int):String;
	/**
	 * Gets the date and time data value of the specified field.
	 * @param i The index of the field to find.
	 * @return The date and time data value of the specified field.
	 */
	function GetDateTime(i:Int):cs.system.DateTime;
	/**
	 * Gets the fixed-position numeric value of the specified field.
	 * @param i The index of the field to find.
	 * @return The fixed-position numeric value of the specified field.
	 */
	function GetDecimal(i:Int):cs.system.Decimal;
	/**
	 * Gets the double-precision floating point number of the specified field.
	 * @param i The index of the field to find.
	 * @return The double-precision floating point number of the specified field.
	 */
	function GetDouble(i:Int):Float;
	/**
	 * Gets the  information corresponding to the type of  that would be returned from
	 * .
	 * @param i The index of the field to find.
	 * @return The  information corresponding to the type of  that would be returned
	 * from .
	 */
	function GetFieldType(i:Int):cs.system.Type;
	/**
	 * Gets the single-precision floating point number of the specified field.
	 * @param i The index of the field to find.
	 * @return The single-precision floating point number of the specified field.
	 */
	function GetFloat(i:Int):Single;
	/**
	 * Returns the GUID value of the specified field.
	 * @param i The index of the field to find.
	 * @return The GUID value of the specified field.
	 */
	function GetGuid(i:Int):cs.system.Guid;
	/**
	 * Gets the 16-bit signed integer value of the specified field.
	 * @param i The index of the field to find.
	 * @return The 16-bit signed integer value of the specified field.
	 */
	function GetInt16(i:Int):cs.Int16;
	/**
	 * Gets the 32-bit signed integer value of the specified field.
	 * @param i The index of the field to find.
	 * @return The 32-bit signed integer value of the specified field.
	 */
	function GetInt32(i:Int):Int;
	/**
	 * Gets the 64-bit signed integer value of the specified field.
	 * @param i The index of the field to find.
	 * @return The 64-bit signed integer value of the specified field.
	 */
	function GetInt64(i:Int):haxe.Int64;
	/**
	 * Gets the name for the field to find.
	 * @param i The index of the field to find.
	 * @return The name of the field or the empty string (""), if there is no value to
	 * return.
	 */
	function GetName(i:Int):String;
	/**
	 * Return the index of the named field.
	 * @param name The name of the field to find.
	 * @return The index of the named field.
	 */
	function GetOrdinal(name:String):Int;
	/**
	 * Gets the string value of the specified field.
	 * @param i The index of the field to find.
	 * @return The string value of the specified field.
	 */
	function GetString(i:Int):String;
	/**
	 * Return the value of the specified field.
	 * @param i The index of the field to find.
	 * @return The  which will contain the field value upon return.
	 */
	function GetValue(i:Int):Dynamic;
	/**
	 * Populates an array of objects with the column values of the current record.
	 * @param values An array of  to copy the attribute fields into.
	 * @return The number of instances of  in the array.
	 */
	function GetValues(values:cs.NativeArray<Dynamic>):Int;
	/**
	 * Return whether the specified field is set to null.
	 * @param i The index of the field to find.
	 * @return if the specified field is set to null; otherwise, .
	 */
	function IsDBNull(i:Int):Bool;
}
