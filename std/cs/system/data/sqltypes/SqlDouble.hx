package cs.system.data.sqltypes;

/** Represents a floating-point number within the range of -1.79E +308 through 1.79E +308 to be stored in or retrieved from a database. */
@:native("System.Data.SqlTypes.SqlDouble")
extern class SqlDouble extends cs.system.ValueType {
	/** A constant representing the maximum value for a  structure. */
	static var MaxValue(default, never):cs.system.data.sqltypes.SqlDouble;
	/** A constant representing the minimum possible value of . */
	static var MinValue(default, never):cs.system.data.sqltypes.SqlDouble;
	/** Represents a  that can be assigned to this instance of the  structure. */
	static var Null(default, never):cs.system.data.sqltypes.SqlDouble;
	/** Represents a zero value that can be assigned to the  property of an instance of the  structure. */
	static var Zero(default, never):cs.system.data.sqltypes.SqlDouble;
	/**
	 * Returns a Boolean value that indicates whether this  instance is null.
	 * @return if  is null. Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the value of the  structure. This property is read-only.
	 * @return The value of the  structure.
	 */
	var Value(default, never):Float;
	function new(value:Float):Void;
	/**
	 * The addition operator computes the sum of the two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The sum of the two  operands.
	 */
	static function Add(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDouble;
	/**
	 * The division operator divides the first  operand by the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the results of the division operation.
	 */
	static function Divide(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDouble;
	/**
	 * Performs a logical comparison on two instances of  to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, .
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
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
	static function GreaterThan(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is greater than or
	 * equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The multiplication operator computes the product of the two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The product of the two  operands.
	 */
	static function Multiply(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDouble;
	/**
	 * Compares two instances of  to determine whether they are notequal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The addition operator computes the sum of the two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The sum of the two  operands.
	 */
	static function op_Addition(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDouble;
	/**
	 * The division operator divides the first  operand by the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the results of the division operation.
	 */
	static function op_Division(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDouble;
	/**
	 * Performs a logical comparison on two instances of  to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlDouble {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDouble):Float {})
	/**
	 * Converts the supplied  parameter to .
	 * @param x The  to be converted.
	 * @return A new  structure whose  is either 0 or 1, depending on the  property of
	 * the  parameter. If the  is , the  structure will be .
	 */
	static function op_Explicit(x:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlDouble;
	/**
	 * Compares two instances of  to determine whether the first is greater than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is greater than or
	 * equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlDouble {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDouble {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlDouble {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt32):cs.system.data.sqltypes.SqlDouble {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt64):cs.system.data.sqltypes.SqlDouble {})
	@:overload(function(x:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlDouble {})
	@:overload(function(x:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlDouble {})
	/**
	 * Converts the supplied  parameter to .
	 * @param x A  structure.
	 * @return A  structure whose  is equal to the  of the  parameter. If the  is , the
	 * structure will be .
	 */
	static function op_Implicit(x:Float):cs.system.data.sqltypes.SqlDouble;
	/**
	 * Compares two instances of  to determine whether they are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The multiplication operator computes the product of the two  operands.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The product of the two  operands.
	 */
	static function op_Multiply(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDouble;
	/**
	 * The subtraction operator the second  operand from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of the subtraction operation.
	 */
	static function op_Subtraction(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDouble;
	/**
	 * Returns the negated value of the specified  structure.
	 * @param x A  structure.
	 * @return A  structure that contains the negated value.
	 */
	static function op_UnaryNegation(x:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDouble;
	/**
	 * Converts the  representation of a number to its double-precision floating point
	 * number equivalent.
	 * @param s The  to be parsed.
	 * @return A  that contains the value represented by the .
	 */
	static function Parse(s:String):cs.system.data.sqltypes.SqlDouble;
	/**
	 * The subtraction operator the second  operand from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The results of the subtraction operation.
	 */
	static function Subtract(x:cs.system.data.sqltypes.SqlDouble, y:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDouble;
	@:overload(function(value:cs.system.data.sqltypes.SqlDouble):Int {})
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
	 * Performs a logical comparison on two instances of  to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for this  structure.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Converts this  structure to .
	 * @return A  structure whose  will be  if the  structure's  is non-zero,  if the 
	 * is zero and  if the  structure is .
	 */
	function ToSqlBoolean():cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts this  structure to .
	 * @return A  structure whose  equals the  of this  structure.
	 */
	function ToSqlByte():cs.system.data.sqltypes.SqlByte;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose converted value equals the rounded value of this
	 * .
	 */
	function ToSqlDecimal():cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the integer part of the  structure's
	 * value.
	 */
	function ToSqlInt16():cs.system.data.sqltypes.SqlInt16;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the integer part of the  structure's
	 * value.
	 */
	function ToSqlInt32():cs.system.data.sqltypes.SqlInt32;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  equals the integer part of the  structure's
	 * value.
	 */
	function ToSqlInt64():cs.system.data.sqltypes.SqlInt64;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  is equal to the value of this .
	 */
	function ToSqlMoney():cs.system.data.sqltypes.SqlMoney;
	/**
	 * Converts this  structure to .
	 * @return A new  structure whose  is equal to the  of this .
	 */
	function ToSqlSingle():cs.system.data.sqltypes.SqlSingle;
	/**
	 * Converts this  structure to .
	 * @return A  representing the  of this .
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Converts this  structure to a string.
	 * @return A string representing the  of this .
	 */
	function ToString():String;
}
