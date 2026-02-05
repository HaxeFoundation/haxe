package cs.system;

/** Provides the base class for enumerations. */
@:native("System.Enum")
extern class Enum extends cs.system.ValueType {
	/**
	 * Converts the specified value of a specified enumerated type to its equivalent
	 * string representation according to the specified format.
	 * @param enumType The enumeration type of the value to convert.
	 * @param value The value to convert.
	 * @param format The output format to use.
	 * @return A string representation of .
	 */
	static function Format(enumType:cs.system.Type, value:Dynamic, format:String):String;
	/**
	 * Retrieves the name of the constant in the specified enumeration that has the
	 * specified value.
	 * @param enumType An enumeration type.
	 * @param value The value of a particular enumerated constant in terms of its
	 * underlying type.
	 * @return A string containing the name of the enumerated constant in  whose value
	 * is ; or  if no such constant is found.
	 */
	static function GetName(enumType:cs.system.Type, value:Dynamic):String;
	/**
	 * Retrieves an array of the names of the constants in a specified enumeration.
	 * @param enumType An enumeration type.
	 * @return A string array of the names of the constants in .
	 */
	static function GetNames(enumType:cs.system.Type):cs.NativeArray<String>;
	/**
	 * Returns the underlying type of the specified enumeration.
	 * @param enumType The enumeration whose underlying type will be retrieved.
	 * @return The underlying type of .
	 */
	static function GetUnderlyingType(enumType:cs.system.Type):cs.system.Type;
	/**
	 * Retrieves an array of the values of the constants in a specified enumeration.
	 * @param enumType An enumeration type.
	 * @return An array that contains the values of the constants in .
	 */
	static function GetValues(enumType:cs.system.Type):cs.system.Array;
	/**
	 * Returns a Boolean telling whether a given integral value, or its name as a
	 * string, exists in a specified enumeration.
	 * @param enumType An enumeration type.
	 * @param value The value or name of a constant in .
	 * @return if a constant in  has a value equal to ; otherwise, .
	 */
	static function IsDefined(enumType:cs.system.Type, value:Dynamic):Bool;
	@:overload(function<TEnum>(value:String):TEnum {})
	@:overload(function(enumType:cs.system.Type, value:String):Dynamic {})
	@:overload(function<TEnum>(value:String, ignoreCase:Bool):TEnum {})
	/**
	 * Converts the string representation of the name or numeric value of one or more
	 * enumerated constants to an equivalent enumerated object.
	 * @param enumType An enumeration type.
	 * @param value A string containing the name or value to convert.
	 * @return An object of type  whose value is represented by .
	 */
	static function Parse(enumType:cs.system.Type, value:String, ignoreCase:Bool):Dynamic;
	@:overload(function(enumType:cs.system.Type, value:cs.UInt8):Dynamic {})
	@:overload(function(enumType:cs.system.Type, value:cs.Int16):Dynamic {})
	@:overload(function(enumType:cs.system.Type, value:Int):Dynamic {})
	@:overload(function(enumType:cs.system.Type, value:haxe.Int64):Dynamic {})
	@:overload(function(enumType:cs.system.Type, value:Dynamic):Dynamic {})
	@:overload(function(enumType:cs.system.Type, value:cs.Int8):Dynamic {})
	@:overload(function(enumType:cs.system.Type, value:cs.UInt16):Dynamic {})
	@:overload(function(enumType:cs.system.Type, value:cs.UInt):Dynamic {})
	/**
	 * Converts the specified 8-bit unsigned integer to an enumeration member.
	 * @param enumType The enumeration type to return.
	 * @param value The value to convert to an enumeration member.
	 * @return An instance of the enumeration set to .
	 */
	static function ToObject(enumType:cs.system.Type, value:cs.UInt64):Dynamic;
	@:overload(function<TEnum>(value:String, result:cs.Ref<TEnum>):Bool {})
	@:overload(function(enumType:cs.system.Type, value:String, result:cs.Ref<Dynamic>):Bool {})
	@:overload(function<TEnum>(value:String, ignoreCase:Bool, result:cs.Ref<TEnum>):Bool {})
	/**
	 * @param enumType 
	 * @param value 
	 * @param ignoreCase 
	 * @param result 
	 */
	static function TryParse(enumType:cs.system.Type, value:String, ignoreCase:Bool, result:cs.Ref<Dynamic>):Bool;
	/**
	 * Compares this instance to a specified object and returns an indication of their
	 * relative values.
	 * @param target An object to compare, or .
	 * @return A signed number that indicates the relative values of this instance and
	 * . Value Meaning Less than zero The value of this instance is less than the value
	 * of . Zero The value of this instance is equal to the value of . Greater than
	 * zero The value of this instance is greater than the value of . -or- is .
	 */
	function CompareTo(target:Dynamic):Int;
	/**
	 * Returns a value indicating whether this instance is equal to a specified object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  is an enumeration value of the same type and with the same
	 * underlying value as this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for the value of this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the type code of the underlying type of this enumeration member.
	 * @return The type code of the underlying type of this instance.
	 */
	function GetTypeCode():cs.system.TypeCode;
	/**
	 * Determines whether one or more bit fields are set in the current instance.
	 * @param flag An enumeration value.
	 * @return if the bit field or bit fields that are set in  are also set in the
	 * current instance; otherwise, .
	 */
	function HasFlag(flag:cs.system.Enum):Bool;
	@:overload(function():String {})
	@:overload(function(provider:cs.system.IFormatProvider):String {})
	@:overload(function(format:String):String {})
	/**
	 * Converts the value of this instance to its equivalent string representation.
	 * @return The string representation of the value of this instance.
	 */
	function ToString(format:String, provider:cs.system.IFormatProvider):String;
}
