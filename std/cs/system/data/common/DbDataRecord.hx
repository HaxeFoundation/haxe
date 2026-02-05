package cs.system.data.common;

/** Implements  and , and provides data binding support for . */
@:native("System.Data.Common.DbDataRecord")
extern class DbDataRecord {
	/**
	 * Indicates the number of fields within the current record. This property is
	 * read-only.
	 * @return The number of fields within the current record.
	 */
	var FieldCount(default, never):Int;
	@:overload(function(index0:Int):Dynamic {})
	@:native("get_Item")
	function get_Item(index0:String):Dynamic;
	/**
	 * Returns the value of the specified column as a Boolean.
	 * @param i The column ordinal.
	 * @return if the Boolean is ; otherwise .
	 */
	function GetBoolean(i:Int):Bool;
	/**
	 * Returns the value of the specified column as a byte.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetByte(i:Int):cs.UInt8;
	/**
	 * Returns the value of the specified column as a byte array.
	 * @param i The zero-based column ordinal.
	 * @param dataIndex The index within the field from which to start the read
	 * operation.
	 * @param buffer The buffer into which to read the stream of bytes.
	 * @param bufferIndex The index for  to start the read operation.
	 * @param length The number of bytes to read.
	 * @return The value of the specified column.
	 */
	function GetBytes(i:Int, dataIndex:haxe.Int64, buffer:cs.NativeArray<cs.UInt8>, bufferIndex:Int, length:Int):haxe.Int64;
	/**
	 * Returns the value of the specified column as a character.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetChar(i:Int):cs.Char16;
	/**
	 * Returns the value of the specified column as a character array.
	 * @param i Column ordinal.
	 * @param dataIndex Buffer to copy data into.
	 * @param buffer Maximum length to copy into the buffer.
	 * @param bufferIndex Point to start from within the buffer.
	 * @param length Point to start from within the source data.
	 * @return The value of the specified column.
	 */
	function GetChars(i:Int, dataIndex:haxe.Int64, buffer:cs.NativeArray<cs.Char16>, bufferIndex:Int, length:Int):haxe.Int64;
	/**
	 * Not currently supported.
	 * @param i Not currently supported.
	 * @return Not currently supported.
	 */
	function GetData(i:Int):cs.system.data.IDataReader;
	/**
	 * Returns the name of the back-end data type.
	 * @param i The column ordinal.
	 * @return The name of the back-end data type.
	 */
	function GetDataTypeName(i:Int):String;
	/**
	 * Returns the value of the specified column as a  object.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetDateTime(i:Int):cs.system.DateTime;
	/**
	 * Returns the value of the specified column as a  object.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetDecimal(i:Int):cs.system.Decimal;
	/**
	 * Returns the value of the specified column as a double-precision floating-point
	 * number.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetDouble(i:Int):Float;
	/**
	 * Returns the  that is the data type of the object.
	 * @param i The column ordinal.
	 * @return The  that is the data type of the object.
	 */
	function GetFieldType(i:Int):cs.system.Type;
	/**
	 * Returns the value of the specified column as a single-precision floating-point
	 * number.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetFloat(i:Int):Single;
	/**
	 * Returns the GUID value of the specified field.
	 * @param i The index of the field to return.
	 * @return The GUID value of the specified field.
	 */
	function GetGuid(i:Int):cs.system.Guid;
	/**
	 * Returns the value of the specified column as a 16-bit signed integer.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetInt16(i:Int):cs.Int16;
	/**
	 * Returns the value of the specified column as a 32-bit signed integer.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetInt32(i:Int):Int;
	/**
	 * Returns the value of the specified column as a 64-bit signed integer.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetInt64(i:Int):haxe.Int64;
	/**
	 * Returns the name of the specified column.
	 * @param i The column ordinal.
	 * @return The name of the specified column.
	 */
	function GetName(i:Int):String;
	/**
	 * Returns the column ordinal, given the name of the column.
	 * @param name The name of the column.
	 * @return The column ordinal.
	 */
	function GetOrdinal(name:String):Int;
	/**
	 * Returns the value of the specified column as a string.
	 * @param i The column ordinal.
	 * @return The value of the specified column.
	 */
	function GetString(i:Int):String;
	/**
	 * Returns the value at the specified column in its native format.
	 * @param i The column ordinal.
	 * @return The value to return.
	 */
	function GetValue(i:Int):Dynamic;
	/**
	 * Populates an array of objects with the column values of the current record.
	 * @param values An array of  to copy the attribute fields into.
	 * @return The number of instances of  in the array.
	 */
	function GetValues(values:cs.NativeArray<Dynamic>):Int;
	/**
	 * Used to indicate nonexistent values.
	 * @param i The column ordinal.
	 * @return if the specified column is equivalent to ; otherwise .
	 */
	function IsDBNull(i:Int):Bool;
}
