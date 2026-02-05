package cs.system.data.sqltypes;

/** Represents an 8-bit unsigned integer, in the range of 0 through 255, to be stored in or retrieved from a database. */
@:native("System.Data.SqlTypes.SqlByte")
extern class SqlByte extends cs.system.ValueType {
	/** A constant representing the largest possible value of a . */
	static var MaxValue(default, never):cs.system.data.sqltypes.SqlByte;
	/** A constant representing the smallest possible value of a . */
	static var MinValue(default, never):cs.system.data.sqltypes.SqlByte;
	/** Represents a  that can be assigned to this instance of the  structure. */
	static var Null(default, never):cs.system.data.sqltypes.SqlByte;
	/** Represents a zero value that can be assigned to the  property of an instance of the  structure. */
	static var Zero(default, never):cs.system.data.sqltypes.SqlByte;
	/**
	 * Gets a Boolean value that indicates whether this  structure is null.
	 * @return if null. Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the value of the  structure. This property is read-only
	 * @return The value of the  structure.
	 */
	var Value(default, never):cs.UInt8;
	function new(value:cs.UInt8):Void;
	/**
	 * Computes the sum of the two specified  structures.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the results of the addition.
	 */
	static function Add(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Computes the bitwise AND of its  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of the bitwise AND operation.
	 */
	static function BitwiseAnd(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Computes the bitwise OR of its two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of the bitwise OR operation.
	 */
	static function BitwiseOr(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Divides its first  operand by its second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the results of the division.
	 */
	static function Divide(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
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
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThan(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is greater than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Computes the remainder after dividing its first  operand by its second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  contains the remainder.
	 */
	static function Mod(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Divides two  values and returns the remainder.
	 * @param x A .
	 * @param y A .
	 * @return The remainder left after division is performed on  and .
	 */
	static function Modulus(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Computes the product of the two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the product of the
	 * multiplication.
	 */
	static function Multiply(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Compares two instances of  to determine whether they are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The ones complement operator performs a bitwise one's complement operation on
	 * its  operand.
	 * @param x A  structure.
	 * @return A  structure whose  property contains the ones complement of the 
	 * parameter.
	 */
	static function OnesComplement(x:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Computes the sum of the two specified  structures.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  whose  property contains the sum of the two operands.
	 */
	static function op_Addition(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Computes the bitwise AND of its  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of the bitwise AND operation.
	 */
	static function op_BitwiseAnd(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Computes the bitwise OR of its two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of the bitwise OR operation.
	 */
	static function op_BitwiseOr(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Divides its first  operand by its second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the results of the division.
	 */
	static function op_Division(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are equal or  if the two instances are
	 * not equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a bitwise exclusive-OR operation on the supplied parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of the bitwise XOR operation.
	 */
	static function op_ExclusiveOr(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	@:overload(function(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlByte {})
	@:overload(function(x:cs.system.data.sqltypes.SqlByte):cs.UInt8 {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlByte {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlByte {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlByte {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt32):cs.system.data.sqltypes.SqlByte {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt64):cs.system.data.sqltypes.SqlByte {})
	@:overload(function(x:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlByte {})
	@:overload(function(x:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlByte {})
	/**
	 * Converts the  parameter to a .
	 * @param x The  parameter to be converted to a .
	 * @return A  whose  property equals the  of the supplied  parameter.
	 */
	static function op_Explicit(x:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlByte;
	/**
	 * Compares two instances of  to determine whether the first is greater than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is greater than or
	 * equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts the supplied byte value to a .
	 * @param x A byte value to be converted to .
	 * @return A  structure whose  property is equal to the supplied parameter.
	 */
	static function op_Implicit(x:cs.UInt8):cs.system.data.sqltypes.SqlByte;
	/**
	 * Compares two instances of  to determine whether they are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Computes the remainder after dividing its first  operand by its second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  contains the remainder.
	 */
	static function op_Modulus(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Computes the product of the two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the product of the
	 * multiplication.
	 */
	static function op_Multiply(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * The ones complement operator performs a bitwise one's complement operation on
	 * its  operand.
	 * @param x A  structure.
	 * @return A  structure whose  property contains the ones complement of the 
	 * parameter.
	 */
	static function op_OnesComplement(x:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Subtracts the second  operand from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of subtracting the second  operand from the first.
	 */
	static function op_Subtraction(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Converts the  representation of a number to its 8-bit unsigned integer
	 * equivalent.
	 * @param s The  to be parsed.
	 * @return A  structure that contains the 8-bit number represented by the 
	 * parameter.
	 */
	static function Parse(s:String):cs.system.data.sqltypes.SqlByte;
	/**
	 * Subtracts the second  operand from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of subtracting the second  operand from the first.
	 */
	static function Subtract(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	/**
	 * Performs a bitwise exclusive-OR operation on the supplied parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of the XOR operation.
	 */
	static function Xor(x:cs.system.data.sqltypes.SqlByte, y:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlByte;
	@:overload(function(value:cs.system.data.sqltypes.SqlByte):Int {})
	/**
	 * Compares this instance to the supplied  object and returns an indication of
	 * their relative values.
	 * @param value The  object to be compared.
	 * @return A signed number that indicates the relative values of the instance and
	 * the object. Return Value Condition Less than zero This instance is less than the
	 * object. Zero This instance is the same as the object. Greater than zero This
	 * instance is greater than the object -or- The object is a null reference ( in
	 * Visual Basic)
	 */
	function CompareTo(value:Dynamic):Int;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Converts this  structure to .
	 * @return if the  is non-zero;  if zero; otherwise Null.
	 */
	function ToSqlBoolean():cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts this  structure to .
	 * @return A  structure whose  equals the  of this  structure.
	 */
	function ToSqlDecimal():cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Converts this  structure to .
	 * @return A  structure with the same value as this .
	 */
	function ToSqlDouble():cs.system.data.sqltypes.SqlDouble;
	/**
	 * Converts this  structure to .
	 * @return A  structure with the same value as this .
	 */
	function ToSqlInt16():cs.system.data.sqltypes.SqlInt16;
	/**
	 * Converts this  to .
	 * @return A  structure with the same value as this .
	 */
	function ToSqlInt32():cs.system.data.sqltypes.SqlInt32;
	/**
	 * Converts this  structure to .
	 * @return A  structure who  equals the  of this .
	 */
	function ToSqlInt64():cs.system.data.sqltypes.SqlInt64;
	/**
	 * Converts this  structure to .
	 * @return A  structure whose  equals the  of this  structure.
	 */
	function ToSqlMoney():cs.system.data.sqltypes.SqlMoney;
	/**
	 * Converts this  structure to .
	 * @return A  structure that has the same  as this  structure.
	 */
	function ToSqlSingle():cs.system.data.sqltypes.SqlSingle;
	/**
	 * Converts this instance of  to .
	 * @return A  that contains the string representation of the  structure's .
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Converts this  structure to a .
	 * @return A string that contains the  of the . If the  is null, the  will be a
	 * null string.
	 */
	function ToString():String;
}
