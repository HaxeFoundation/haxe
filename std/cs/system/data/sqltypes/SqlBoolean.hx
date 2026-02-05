package cs.system.data.sqltypes;

/** Represents an integer value that is either 1 or 0 to be stored in or retrieved from a database. */
@:native("System.Data.SqlTypes.SqlBoolean")
extern class SqlBoolean extends cs.system.ValueType {
	/** Represents a false value that can be assigned to the  property of an instance of the  structure. */
	static var False(default, never):cs.system.data.sqltypes.SqlBoolean;
	/** Represents  that can be assigned to this instance of the  structure. */
	static var Null(default, never):cs.system.data.sqltypes.SqlBoolean;
	/** Represents a one value that can be assigned to the  property of an instance of the  structure. */
	static var One(default, never):cs.system.data.sqltypes.SqlBoolean;
	/** Represents a true value that can be assigned to the  property of an instance of the  structure. */
	static var True(default, never):cs.system.data.sqltypes.SqlBoolean;
	/** Represents a zero value that can be assigned to the  property of an instance of the  structure. */
	static var Zero(default, never):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Gets the value of the  structure as a byte.
	 * @return A byte representing the value of the  structure.
	 */
	var ByteValue(default, never):cs.UInt8;
	/**
	 * Indicates whether the current  is .
	 * @return if  is ; otherwise, .
	 */
	var IsFalse(default, never):Bool;
	/**
	 * Indicates whether this  structure is null.
	 * @return if the  structure is null; otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current  is .
	 * @return if  is ; otherwise, .
	 */
	var IsTrue(default, never):Bool;
	/**
	 * Gets the  structure's value. This property is read-only.
	 * @return if the  is ; otherwise, .
	 */
	var Value(default, never):Bool;
	@:overload(function(value:Bool):Void {})
	function new(value:Int):Void;
	/**
	 * Computes the bitwise AND operation of two specified  structures.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The result of the logical AND operation.
	 */
	static function And(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether they are equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two instances are equal or  if the two instances are not equal.
	 * If either instance of  is null, the  of the  will be .
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet A .
	 * @return A  value that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	/**
	 * Compares two instances of  to determine whether the first is greater than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the first instance is greater than the second instance; otherwise, .
	 */
	static function GreaterThan(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is greater than or
	 * equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the first instance is greater than or equal to the second instance;
	 * otherwise, .
	 */
	static function GreaterThanOrEquals(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the first instance is less than the second instance; otherwise, .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the first instance is less than or equal to the second instance;
	 * otherwise, .
	 */
	static function LessThanOrEquals(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  for equality.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two instances are not equal or  if the two instances are equal.
	 * If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a one's complement operation on the supplied  structures.
	 * @param x A  structure.
	 * @return The one's complement of the supplied .
	 */
	static function OnesComplement(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Computes the bitwise AND operation of two specified  structures.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The result of the logical AND operation.
	 */
	static function op_BitwiseAnd(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Computes the bitwise OR of its operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of the logical OR operation.
	 */
	static function op_BitwiseOr(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  for equality.
	 * @param x A .
	 * @param y A .
	 * @return if the two instances are equal or  if the two instances are not equal.
	 * If either instance of  is null, the  of the  will be .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a bitwise exclusive-OR (XOR) operation on the supplied parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The result of the logical XOR operation.
	 */
	static function op_ExclusiveOr(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlBoolean):Bool {})
	@:overload(function(x:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt32):cs.system.data.sqltypes.SqlBoolean {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt64):cs.system.data.sqltypes.SqlBoolean {})
	@:overload(function(x:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean {})
	@:overload(function(x:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean {})
	/**
	 * Converts a  to a Boolean.
	 * @param x A  to convert.
	 * @return A Boolean set to the  of the .
	 */
	static function op_Explicit(x:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The false operator can be used to test the  of the  to determine whether it is
	 * false.
	 * @param x The  structure to be tested.
	 * @return if the supplied parameter is  is ; otherwise, .
	 */
	static function op_False(x:cs.system.data.sqltypes.SqlBoolean):Bool;
	/**
	 * Compares two  structures to determine whether the first is greater than the
	 * second.
	 * @param x A  object.
	 * @param y A  object.
	 * @return if the first instance is greater than the second instance; otherwise, .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is greater than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the first instance is greater than or equal to the second instance;
	 * otherwise, .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts the supplied byte value to a .
	 * @param x A byte value to be converted to .
	 * @return A  value that contains 0 or 1.
	 */
	static function op_Implicit(x:Bool):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether they are not equal.
	 * @param x A .
	 * @param y A .
	 * @return if the two instances are not equal or  if the two instances are equal.
	 * If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the first instance is less than the second instance; otherwise, .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the first instance is less than or equal to the second instance;
	 * otherwise, .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a NOT operation on a .
	 * @param x The  on which the NOT operation will be performed.
	 * @return A  with the  if argument was true,  if argument was null, and 
	 * otherwise.
	 */
	static function op_LogicalNot(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a one's complement operation on the supplied  structures.
	 * @param x A  structure.
	 * @return The one's complement of the supplied .
	 */
	static function op_OnesComplement(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The true operator can be used to test the  of the  to determine whether it is
	 * true.
	 * @param x The  structure to be tested.
	 * @return if the supplied parameter is  is ; otherwise, .
	 */
	static function op_True(x:cs.system.data.sqltypes.SqlBoolean):Bool;
	/**
	 * Performs a bitwise OR operation on the two specified  structures.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose Value is the result of the bitwise OR operation.
	 */
	static function Or(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts the specified  representation of a logical value to its  equivalent.
	 * @param s The  to be converted.
	 * @return A  structure that contains the parsed value.
	 */
	static function Parse(s:String):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a bitwise exclusive-OR operation on the supplied parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The result of the logical XOR operation.
	 */
	static function Xor(x:cs.system.data.sqltypes.SqlBoolean, y:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(value:cs.system.data.sqltypes.SqlBoolean):Int {})
	/**
	 * Compares this  object to the supplied  object and returns an indication of their
	 * relative values.
	 * @param value A  object to compare, or a null reference ( in Visual Basic).
	 * @return A signed number that indicates the relative values of the instance and
	 * value. Value Description A negative integer This instance is less than . Zero
	 * This instance is equal to . A positive integer This instance is greater than .
	 * -or- is a null reference ( in Visual Basic).
	 */
	function CompareTo(value:Dynamic):Int;
	/**
	 * Compares two  structures to determine whether they are equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two instances are equal or  if the two instances are not equal.
	 * If either instance of  is null, the  of the  will be .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose value is 1 or 0. If the  structure's value equals
	 * , the new  structure's value is 1. Otherwise, the new  structure's value is 0.
	 */
	function ToSqlByte():cs.system.data.sqltypes.SqlByte;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose value is 1 or 0. If the  structure's value equals
	 * then the new  structure's value is 1. Otherwise, the new  structure's value is
	 * 0.
	 */
	function ToSqlDecimal():cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose value is 1 or 0. If the  structure's value equals
	 * then the new  structure's value is 1. Otherwise, the new  structure's value is
	 * 0.
	 */
	function ToSqlDouble():cs.system.data.sqltypes.SqlDouble;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose value is 1 or 0. If the  structure's value equals
	 * then the new  structure's value is 1. Otherwise, the new  structure's value is
	 * 0.
	 */
	function ToSqlInt16():cs.system.data.sqltypes.SqlInt16;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose value is 1 or 0. If the  structure's value equals
	 * , the new  structure's value is 1. Otherwise, the new  structure's value is 0.
	 */
	function ToSqlInt32():cs.system.data.sqltypes.SqlInt32;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose value is 1 or 0. If the  structure's value equals
	 * , the new  structure's value is 1. Otherwise, the new  structure's value is 0.
	 */
	function ToSqlInt64():cs.system.data.sqltypes.SqlInt64;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose value is 1 or 0. If the  structure's value equals
	 * , the new  value is 1. If the  structure's value equals , the new  value is 0.
	 * If  structure's value is neither 1 nor 0, the new  value is .
	 */
	function ToSqlMoney():cs.system.data.sqltypes.SqlMoney;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose value is 1 or 0. If the  structure's value equals
	 * true, the new  structure's value is 1; otherwise the new  structure's value is
	 * 0.
	 */
	function ToSqlSingle():cs.system.data.sqltypes.SqlSingle;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose value is 1 or 0. If the  structure's value equals
	 * then  structure's value is 1. Otherwise, the new  structure's value is 0.
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Converts this  structure to a string.
	 * @return A string that contains the value of the . If the value is null, the
	 * string will contain "null".
	 */
	function ToString():String;
}
