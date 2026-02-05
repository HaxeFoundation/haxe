package cs.system.data;

@:native("System.Data.DataReaderExtensions")
extern class DataReaderExtensions {
	static function GetBoolean(reader:cs.system.data.common.DbDataReader, name:String):Bool;
	static function GetByte(reader:cs.system.data.common.DbDataReader, name:String):cs.UInt8;
	static function GetBytes(reader:cs.system.data.common.DbDataReader, name:String, dataOffset:haxe.Int64, buffer:cs.NativeArray<cs.UInt8>, bufferOffset:Int, length:Int):haxe.Int64;
	static function GetChar(reader:cs.system.data.common.DbDataReader, name:String):cs.Char16;
	static function GetChars(reader:cs.system.data.common.DbDataReader, name:String, dataOffset:haxe.Int64, buffer:cs.NativeArray<cs.Char16>, bufferOffset:Int, length:Int):haxe.Int64;
	static function GetData(reader:cs.system.data.common.DbDataReader, name:String):cs.system.data.common.DbDataReader;
	static function GetDataTypeName(reader:cs.system.data.common.DbDataReader, name:String):String;
	static function GetDateTime(reader:cs.system.data.common.DbDataReader, name:String):cs.system.DateTime;
	static function GetDecimal(reader:cs.system.data.common.DbDataReader, name:String):cs.system.Decimal;
	static function GetDouble(reader:cs.system.data.common.DbDataReader, name:String):Float;
	static function GetFieldType(reader:cs.system.data.common.DbDataReader, name:String):cs.system.Type;
	static function GetFieldValue<T>(reader:cs.system.data.common.DbDataReader, name:String):T;
	static function GetFieldValueAsync<T>(reader:cs.system.data.common.DbDataReader, name:String, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<T>;
	static function GetFloat(reader:cs.system.data.common.DbDataReader, name:String):Single;
	static function GetGuid(reader:cs.system.data.common.DbDataReader, name:String):cs.system.Guid;
	static function GetInt16(reader:cs.system.data.common.DbDataReader, name:String):cs.Int16;
	static function GetInt32(reader:cs.system.data.common.DbDataReader, name:String):Int;
	static function GetInt64(reader:cs.system.data.common.DbDataReader, name:String):haxe.Int64;
	static function GetProviderSpecificFieldType(reader:cs.system.data.common.DbDataReader, name:String):cs.system.Type;
	static function GetProviderSpecificValue(reader:cs.system.data.common.DbDataReader, name:String):Dynamic;
	static function GetStream(reader:cs.system.data.common.DbDataReader, name:String):cs.system.io.Stream;
	static function GetString(reader:cs.system.data.common.DbDataReader, name:String):String;
	static function GetTextReader(reader:cs.system.data.common.DbDataReader, name:String):cs.system.io.TextReader;
	static function GetValue(reader:cs.system.data.common.DbDataReader, name:String):Dynamic;
	static function IsDBNull(reader:cs.system.data.common.DbDataReader, name:String):Bool;
	static function IsDBNullAsync(reader:cs.system.data.common.DbDataReader, name:String, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Bool>;
}
