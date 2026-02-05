package cs.system.runtime.serialization;

/** Stores all the data needed to serialize or deserialize an object. This class cannot be inherited. */
@:native("System.Runtime.Serialization.SerializationInfo")
extern class SerializationInfo {
	/**
	 * Gets or sets the assembly name of the type to serialize during serialization
	 * only.
	 * @return The full name of the assembly of the type to serialize.
	 */
	var AssemblyName(default, default):String;
	/**
	 * Gets or sets the full name of the  to serialize.
	 * @return The full name of the type to serialize.
	 */
	var FullTypeName(default, default):String;
	/**
	 * Gets whether the assembly name has been explicitly set.
	 * @return if the assembly name has been explicitly set; otherwise, .
	 */
	var IsAssemblyNameSetExplicit(default, never):Bool;
	/**
	 * Gets whether the full type name has been explicitly set.
	 * @return if the full type name has been explicitly set; otherwise, .
	 */
	var IsFullTypeNameSetExplicit(default, never):Bool;
	/**
	 * Gets the number of members that have been added to the  store.
	 * @return The number of members that have been added to the current .
	 */
	var MemberCount(default, never):Int;
	/**
	 * Returns the type of the object to be serialized.
	 * @return The type of the object being serialized.
	 */
	var ObjectType(default, never):cs.system.Type;
	@:overload(function(type:cs.system.Type, converter:cs.system.runtime.serialization.IFormatterConverter):Void {})
	function new(type:cs.system.Type, converter:cs.system.runtime.serialization.IFormatterConverter, requireSameTokenInPartialTrust:Bool):Void;
	@:overload(function(name:String, value:Bool):Void {})
	@:overload(function(name:String, value:cs.UInt8):Void {})
	@:overload(function(name:String, value:cs.Char16):Void {})
	@:overload(function(name:String, value:cs.system.DateTime):Void {})
	@:overload(function(name:String, value:cs.system.Decimal):Void {})
	@:overload(function(name:String, value:Float):Void {})
	@:overload(function(name:String, value:cs.Int16):Void {})
	@:overload(function(name:String, value:Int):Void {})
	@:overload(function(name:String, value:haxe.Int64):Void {})
	@:overload(function(name:String, value:Dynamic):Void {})
	@:overload(function(name:String, value:cs.Int8):Void {})
	@:overload(function(name:String, value:Single):Void {})
	@:overload(function(name:String, value:cs.UInt16):Void {})
	@:overload(function(name:String, value:cs.UInt):Void {})
	@:overload(function(name:String, value:cs.UInt64):Void {})
	/**
	 * Adds a Boolean value into the  store.
	 * @param name The name to associate with the value, so it can be deserialized
	 * later.
	 * @param value The Boolean value to serialize.
	 */
	function AddValue(name:String, value:Dynamic, type:cs.system.Type):Void;
	/**
	 * Retrieves a Boolean value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The Boolean value associated with .
	 */
	function GetBoolean(name:String):Bool;
	/**
	 * Retrieves an 8-bit unsigned integer value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The 8-bit unsigned integer associated with .
	 */
	function GetByte(name:String):cs.UInt8;
	/**
	 * Retrieves a Unicode character value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The Unicode character associated with .
	 */
	function GetChar(name:String):cs.Char16;
	/**
	 * Retrieves a  value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The  value associated with .
	 */
	function GetDateTime(name:String):cs.system.DateTime;
	/**
	 * Retrieves a decimal value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return A decimal value from the .
	 */
	function GetDecimal(name:String):cs.system.Decimal;
	/**
	 * Retrieves a double-precision floating-point value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The double-precision floating-point value associated with .
	 */
	function GetDouble(name:String):Float;
	/**
	 * Returns a  used to iterate through the name-value pairs in the  store.
	 * @return A  for parsing the name-value pairs contained in the  store.
	 */
	function GetEnumerator():cs.system.runtime.serialization.SerializationInfoEnumerator;
	/**
	 * Retrieves a 16-bit signed integer value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The 16-bit signed integer associated with .
	 */
	function GetInt16(name:String):cs.Int16;
	/**
	 * Retrieves a 32-bit signed integer value from the  store.
	 * @param name The name of the value to retrieve.
	 * @return The 32-bit signed integer associated with .
	 */
	function GetInt32(name:String):Int;
	/**
	 * Retrieves a 64-bit signed integer value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The 64-bit signed integer associated with .
	 */
	function GetInt64(name:String):haxe.Int64;
	/**
	 * Retrieves an 8-bit signed integer value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The 8-bit signed integer associated with .
	 */
	function GetSByte(name:String):cs.Int8;
	/**
	 * Retrieves a single-precision floating-point value from the  store.
	 * @param name The name of the value to retrieve.
	 * @return The single-precision floating-point value associated with .
	 */
	function GetSingle(name:String):Single;
	/**
	 * Retrieves a  value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The  associated with .
	 */
	function GetString(name:String):String;
	/**
	 * Retrieves a 16-bit unsigned integer value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The 16-bit unsigned integer associated with .
	 */
	function GetUInt16(name:String):cs.UInt16;
	/**
	 * Retrieves a 32-bit unsigned integer value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The 32-bit unsigned integer associated with .
	 */
	function GetUInt32(name:String):cs.UInt;
	/**
	 * Retrieves a 64-bit unsigned integer value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @return The 64-bit unsigned integer associated with .
	 */
	function GetUInt64(name:String):cs.UInt64;
	/**
	 * Retrieves a value from the  store.
	 * @param name The name associated with the value to retrieve.
	 * @param type The  of the value to retrieve. If the stored value cannot be
	 * converted to this type, the system will throw a .
	 * @return The object of the specified  associated with .
	 */
	function GetValue(name:String, type:cs.system.Type):Dynamic;
	/**
	 * Sets the  of the object to serialize.
	 * @param type The  of the object to serialize.
	 */
	function SetType(type:cs.system.Type):Void;
}
