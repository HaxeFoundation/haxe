package cs.system.data.sqltypes;

/** Represents a 16-bit signed integer to be stored in or retrieved from a database. */
@:native("System.Data.SqlTypes.SqlInt16")
extern class SqlInt16 extends cs.system.ValueType {
	/** A constant representing the largest possible value of a . */
	static var MaxValue(default, never):cs.system.data.sqltypes.SqlInt16;
	/** A constant representing the smallest possible value of a . */
	static var MinValue(default, never):cs.system.data.sqltypes.SqlInt16;
	/** Represents a  that can be assigned to this instance of the  structure. */
	static var Null(default, never):cs.system.data.sqltypes.SqlInt16;
	/** Represents a zero value that can be assigned to the  property of an instance of the  structure. */
	static var Zero(default, never):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Indicates whether this  structure is null.
	 * @return if null. Otherwise, . For more information, see Handling Null Values.
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the value of this instance of  structure. This property is read-only.
	 * @return A short integer representing the value of this  structure.
	 */
	var Value(default, never):cs.Int16;
	function new(value:cs.Int16):Void;
	/**
	 * Computes the sum of the two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the sum of the two  operands.
	 */
	static function Add(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Computes the bitwise AND of its  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the results of the bitwise AND.
	 */
	static function BitwiseAnd(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Computes the bitwise OR of its two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the results of the bitwise OR.
	 */
	static function BitwiseOr(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Divides the first  operand by the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  whose  property contains the results of the division.
	 */
	static function Divide(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet An .
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
	static function GreaterThan(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is greater than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is less than or equal to
	 * the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Computes the remainder after dividing its first  operand by its second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  contains the remainder.
	 */
	static function Mod(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Divides two  values and returns the remainder.
	 * @param x A  value.
	 * @param y A  value.
	 * @return The remainder left after division is performed on  and .
	 */
	static function Modulus(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Computes the product of the two  parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  contains the product of the two parameters.
	 */
	static function Multiply(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The ~ operator performs a bitwise one's complement operation on its  operand.
	 * @param x A  structure.
	 * @return A  structure whose  property contains the complement of the specified 
	 * structure.
	 */
	static function OnesComplement(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Computes the sum of the two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the sum of the two  operands.
	 */
	static function op_Addition(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Computes the bitwise AND of its  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the results of the bitwise AND.
	 */
	static function op_BitwiseAnd(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Computes the bitwise OR of its two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the results of the bitwise OR.
	 */
	static function op_BitwiseOr(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Divides the first  operand by the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  whose  property contains the results of the division.
	 */
	static function op_Division(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are equal or  if the two instances are
	 * not equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a bitwise exclusive-OR operation on the supplied parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the results of the bitwise XOR.
	 */
	static function op_ExclusiveOr(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	@:overload(function(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlInt16 {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlInt16 {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlInt16 {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt16):cs.Int16 {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt32):cs.system.data.sqltypes.SqlInt16 {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt64):cs.system.data.sqltypes.SqlInt16 {})
	@:overload(function(x:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlInt16 {})
	@:overload(function(x:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlInt16 {})
	/**
	 * Converts the supplied  structure to .
	 * @param x A  structure.
	 * @return A new  structure whose  property is equal to the  property of the 
	 * parameter.
	 */
	static function op_Explicit(x:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Compares two instances of  to determine whether the first is greater than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is greater than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlInt16 {})
	/**
	 * Converts the supplied  structure to .
	 * @param x A  structure.
	 * @return A new  structure whose  property is equal to the  property of the 
	 * parameter.
	 */
	static function op_Implicit(x:cs.Int16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is less than or equal to
	 * the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Computes the remainder after dividing its first  operand by its second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  contains the remainder.
	 */
	static function op_Modulus(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Computes the product of the two  parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  contains the product of the two parameters.
	 */
	static function op_Multiply(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * The ~ operator performs a bitwise one's complement operation on its  operand.
	 * @param x A  structure.
	 * @return A  structure whose  property contains the complement of the specified 
	 * structure.
	 */
	static function op_OnesComplement(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Subtracts the second  parameter from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the results of the subtraction.
	 */
	static function op_Subtraction(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * The unary minus operator negates the  of the  operand.
	 * @param x A  structure.
	 * @return A  structure that contains the negated value.
	 */
	static function op_UnaryNegation(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Converts the  representation of a number to its 16-bit signed integer
	 * equivalent.
	 * @param s The  to be parsed.
	 * @return A 16-bit signed integer equivalent to the value that is contained in the
	 * specified .
	 */
	static function Parse(s:String):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Subtracts the second  parameter from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure whose  property contains the results of the subtraction.
	 */
	static function Subtract(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	/**
	 * Performs a bitwise exclusive-OR operation on the supplied parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the results of the XOR operation.
	 */
	static function Xor(x:cs.system.data.sqltypes.SqlInt16, y:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlInt16;
	@:overload(function(value:cs.system.data.sqltypes.SqlInt16):Int {})
	/**
	 * Compares this  instance to the supplied  and returns an indication of their
	 * relative values.
	 * @param value The  to be compared.
	 * @return A signed number that indicates the relative values of the instance and
	 * the object. Return value Condition Less than zero This instance is less than the
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
	 * @return A  structure whose  equals the  of this  structure. If the value of the 
	 * is less than 0 or greater than 255, an  occurs.
	 */
	function ToSqlByte():cs.system.data.sqltypes.SqlByte;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the value of this  structure.
	 */
	function ToSqlDecimal():cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the value of this  structure.
	 */
	function ToSqlDouble():cs.system.data.sqltypes.SqlDouble;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the value of this  structure.
	 */
	function ToSqlInt32():cs.system.data.sqltypes.SqlInt32;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the value of this  structure.
	 */
	function ToSqlInt64():cs.system.data.sqltypes.SqlInt64;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the value of this  structure.
	 */
	function ToSqlMoney():cs.system.data.sqltypes.SqlMoney;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the value of this  structure.
	 */
	function ToSqlSingle():cs.system.data.sqltypes.SqlSingle;
	/**
	 * Converts this  structure to .
	 * @return A  representing the  of this instance of .
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Converts a  structure to .
	 * @return A  object representing the  of this instance of .
	 */
	function ToString():String;
}
