package cs.system.data.sqltypes;

/** Represents a variable-length stream of characters to be stored in or retrieved from the database.  has a different underlying data structure from its corresponding .NET Framework  data type. */
@:native("System.Data.SqlTypes.SqlString")
extern class SqlString extends cs.system.ValueType {
	/** Specifies that sorts should be based on a characters numeric value instead of its alphabetical value. */
	static var BinarySort(default, never):Int;
	/** Specifies that sorts should be based on a character's numeric value instead of its alphabetical value. */
	static var BinarySort2(default, never):Int;
	/** Specifies that  comparisons should ignore case. */
	static var IgnoreCase(default, never):Int;
	/** Specifies that the string comparison must ignore the Kana type. */
	static var IgnoreKanaType(default, never):Int;
	/** Specifies that the string comparison must ignore non-space combining characters, such as diacritics. */
	static var IgnoreNonSpace(default, never):Int;
	/** Specifies that the string comparison must ignore the character width. */
	static var IgnoreWidth(default, never):Int;
	/** Represents a  that can be assigned to this instance of the  structure. */
	static var Null(default, never):cs.system.data.sqltypes.SqlString;
	/**
	 * Gets the  object that defines how string comparisons should be performed for
	 * this  structure.
	 * @return A  object that defines string comparison for this  structure.
	 */
	var CompareInfo(default, never):cs.system.globalization.CompareInfo;
	/**
	 * Gets the  structure that represents information about the culture of this 
	 * object.
	 * @return A  structure that describes information about the culture of this
	 * SqlString structure including the names of the culture, the writing system, and
	 * the calendar used, and also access to culture-specific objects that provide
	 * methods for common operations, such as formatting dates and sorting strings.
	 */
	var CultureInfo(default, never):cs.system.globalization.CultureInfo;
	/**
	 * Indicates whether this  structure is null.
	 * @return if  is . Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Specifies the geographical locale and language for the  structure.
	 * @return The locale id for the string stored in the  property.
	 */
	var LCID(default, never):Int;
	/**
	 * A combination of one or more of the  enumeration values that represent the way
	 * in which this  should be compared to other  structures.
	 * @return A value specifying how this  should be compared to other  structures.
	 */
	var SqlCompareOptions(default, never):cs.system.data.sqltypes.SqlCompareOptions;
	/**
	 * Gets the string that is stored in this  structure. This property is read-only.
	 * @return The string that is stored.
	 */
	var Value(default, never):String;
	@:overload(function(data:String):Void {})
	@:overload(function(data:String, lcid:Int):Void {})
	@:overload(function(lcid:Int, compareOptions:cs.system.data.sqltypes.SqlCompareOptions, data:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(data:String, lcid:Int, compareOptions:cs.system.data.sqltypes.SqlCompareOptions):Void {})
	@:overload(function(lcid:Int, compareOptions:cs.system.data.sqltypes.SqlCompareOptions, data:cs.NativeArray<cs.UInt8>, fUnicode:Bool):Void {})
	@:overload(function(lcid:Int, compareOptions:cs.system.data.sqltypes.SqlCompareOptions, data:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Void {})
	function new(lcid:Int, compareOptions:cs.system.data.sqltypes.SqlCompareOptions, data:cs.NativeArray<cs.UInt8>, index:Int, count:Int, fUnicode:Bool):Void;
	/**
	 * Concatenates two specified  values to create a new  structure.
	 * @param x A .
	 * @param y A .
	 * @return A  that is the concatenated value of  and .
	 */
	static function Add(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlString;
	/**
	 * Gets the  enumeration equivalent of the specified  value.
	 * @param compareOptions A  value that describes the comparison options for this 
	 * structure.
	 * @return A  value that corresponds to the  for this  structure.
	 */
	static function CompareOptionsFromSqlCompareOptions(compareOptions:cs.system.data.sqltypes.SqlCompareOptions):cs.system.globalization.CompareOptions;
	/**
	 * Concatenates the two specified  structures.
	 * @param x A .
	 * @param y A .
	 * @return A  that contains the newly concatenated value representing the contents
	 * of the two  parameters.
	 */
	static function Concat(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlString;
	/**
	 * Performs a logical comparison of the two  operands to determine whether they are
	 * equal.
	 * @param x A .
	 * @param y A .
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet A .
	 * @return A  value that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is greater than the second.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThan(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is greater than or equal to the second.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is less than the second.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is less than or equal to the second.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  operands to determine whether they are
	 * not equal.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Concatenates the two specified  structures.
	 * @param x A .
	 * @param y A .
	 * @return A  that contains the newly concatenated value representing the contents
	 * of the two  parameters.
	 */
	static function op_Addition(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlString;
	/**
	 * Performs a logical comparison of the two  operands to determine whether they are
	 * equal.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the two instances are equal or  if the two instances are
	 * not equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt32):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt64):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlString {})
	@:overload(function(x:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlString {})
	/**
	 * Converts the specified  structure to .
	 * @param x The  structure to be converted.
	 * @return A new  that contains the string representation of the  parameter.
	 */
	static function op_Explicit(x:cs.system.data.sqltypes.SqlString):String;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is greater than the second.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is greater than or equal to the second.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts the  parameter to a .
	 * @param x The  to be converted.
	 * @return A  that contains the value of the specified .
	 */
	static function op_Implicit(x:String):cs.system.data.sqltypes.SqlString;
	/**
	 * Performs a logical comparison of the two  operands to determine whether they are
	 * not equal.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is less than the second.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is less than or equal to the second.
	 * @param x A .
	 * @param y A .
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlString, y:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Creates a copy of this  object.
	 * @return A new  object in which all property values are the same as the original.
	 */
	function Clone():cs.system.data.sqltypes.SqlString;
	@:overload(function(value:cs.system.data.sqltypes.SqlString):Int {})
	/**
	 * Compares this  instance to the supplied  and returns an indication of their
	 * relative values.
	 * @param value The  to be compared.
	 * @return A signed number that indicates the relative values of the instance and
	 * the object. Return value Condition Less than zero This instance is less than the
	 * object. Zero This instance is the same as the object. Greater than zero This
	 * instance is greater than the object -or- The object is a null reference ( in
	 * Visual Basic).
	 */
	function CompareTo(value:Dynamic):Int;
	/**
	 * Performs a logical comparison of the two  operands to determine whether they are
	 * equal.
	 * @param x A .
	 * @param y A .
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Gets the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Gets an array of bytes, that contains the contents of the  in ANSI format.
	 * @return An byte array, that contains the contents of the  in ANSI format.
	 */
	function GetNonUnicodeBytes():cs.NativeArray<cs.UInt8>;
	/**
	 * Gets an array of bytes, that contains the contents of the  in Unicode format.
	 * @return An byte array, that contains the contents of the  in Unicode format.
	 */
	function GetUnicodeBytes():cs.NativeArray<cs.UInt8>;
	/**
	 * Converts this  structure to .
	 * @return if the  is non-zero;  if zero; otherwise Null.
	 */
	function ToSqlBoolean():cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the number represented by this 
	 * structure.
	 */
	function ToSqlByte():cs.system.data.sqltypes.SqlByte;
	/**
	 * Converts this  structure to .
	 * @return A new  structure that contains the date value represented by this .
	 */
	function ToSqlDateTime():cs.system.data.sqltypes.SqlDateTime;
	/**
	 * Converts this  structure to .
	 * @return A new  that contains the value of this .
	 */
	function ToSqlDecimal():cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Converts this  structure to .
	 * @return A new  that is equal to the numeric value of this .
	 */
	function ToSqlDouble():cs.system.data.sqltypes.SqlDouble;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  is the  represented by this  structure.
	 */
	function ToSqlGuid():cs.system.data.sqltypes.SqlGuid;
	/**
	 * Converts this  structure to .
	 * @return A new  that is equal to the numeric value of this .
	 */
	function ToSqlInt16():cs.system.data.sqltypes.SqlInt16;
	/**
	 * Converts this  structure to .
	 * @return A new  that is equal to the numeric value of this .
	 */
	function ToSqlInt32():cs.system.data.sqltypes.SqlInt32;
	/**
	 * Converts this  structure to .
	 * @return A new  that is equal to the numeric value of this .
	 */
	function ToSqlInt64():cs.system.data.sqltypes.SqlInt64;
	/**
	 * Converts this  structure to .
	 * @return A new  that is equal to the numeric value of this .
	 */
	function ToSqlMoney():cs.system.data.sqltypes.SqlMoney;
	/**
	 * Converts this  structure to .
	 * @return A new  that is equal to the numeric value of this .
	 */
	function ToSqlSingle():cs.system.data.sqltypes.SqlSingle;
	/**
	 * Converts a  object to a .
	 * @return A  with the same value as this  structure.
	 */
	function ToString():String;
}
