package cs.system.data.sqltypes;

/** Represents a numeric value between - 10^38 +1 and 10^38 - 1, with fixed precision and scale. */
@:native("System.Data.SqlTypes.SqlDecimal")
extern class SqlDecimal extends cs.system.ValueType {
	/** A constant representing the largest possible value for the  property. */
	static var MaxPrecision(default, never):cs.UInt8;
	/** A constant representing the maximum value for the  property. */
	static var MaxScale(default, never):cs.UInt8;
	/** A constant representing the maximum value of a  structure. */
	static var MaxValue(default, never):cs.system.data.sqltypes.SqlDecimal;
	/** A constant representing the minimum value for a  structure. */
	static var MinValue(default, never):cs.system.data.sqltypes.SqlDecimal;
	/** Represents a  that can be assigned to this instance of the  class. */
	static var Null(default, never):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Gets the binary representation of the value of this  structure as an array of
	 * bytes.
	 * @return An array of bytes that contains the binary representation of the 
	 * structure's value.
	 */
	var BinData(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the binary representation of this  structure as an array of integers.
	 * @return An array of integers that contains the binary representation of this 
	 * structure.
	 */
	var Data(default, never):cs.NativeArray<Int>;
	/**
	 * Indicates whether this  structure is null.
	 * @return if this  structure is null. Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Indicates whether the  of this  structure is greater than zero.
	 * @return if the  is assigned to null. Otherwise, .
	 */
	var IsPositive(default, never):Bool;
	/**
	 * Gets the maximum number of digits used to represent the  property.
	 * @return The maximum number of digits used to represent the  of this  structure.
	 */
	var Precision(default, never):cs.UInt8;
	/**
	 * Gets the number of decimal places to which  is resolved.
	 * @return The number of decimal places to which the  property is resolved.
	 */
	var Scale(default, never):cs.UInt8;
	/**
	 * Gets the value of the  structure. This property is read-only.
	 * @return A number in the range -79,228,162,514,264,337,593,543,950,335 through
	 * 79,228,162,514,162,514,264,337,593,543,950,335.
	 */
	var Value(default, never):cs.system.Decimal;
	@:overload(function(value:cs.system.Decimal):Void {})
	@:overload(function(dVal:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(bPrecision:cs.UInt8, bScale:cs.UInt8, fPositive:Bool, bits:cs.NativeArray<Int>):Void {})
	function new(bPrecision:cs.UInt8, bScale:cs.UInt8, fPositive:Bool, data1:Int, data2:Int, data3:Int, data4:Int):Void;
	/**
	 * The Abs method gets the absolute value of the  parameter.
	 * @param n A  structure.
	 * @return A  structure whose  property contains the unsigned number representing
	 * the absolute value of the  parameter.
	 */
	static function Abs(n:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Calculates the sum of the two  operators.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the sum.
	 */
	static function Add(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * The scale of the  operand will be adjusted to the number of digits indicated by
	 * the digits parameter. Depending on the value of the fRound parameter, the value
	 * will either be rounded to the appropriate number of digits or truncated.
	 * @param n The  structure to be adjusted.
	 * @param digits The number of digits in the adjusted structure.
	 * @param fRound If this parameter is , the new Value will be rounded, if , the
	 * value will be truncated.
	 * @return A new  structure whose  property contains the adjusted number.
	 */
	static function AdjustScale(n:cs.system.data.sqltypes.SqlDecimal, digits:Int, fRound:Bool):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Returns the smallest whole number greater than or equal to the specified 
	 * structure.
	 * @param n The  structure for which the ceiling value is to be calculated.
	 * @return A  representing the smallest whole number greater than or equal to the
	 * specified  structure.
	 */
	static function Ceiling(n:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Adjusts the value of the  operand to the indicated precision and scale.
	 * @param n The  structure whose value is to be adjusted.
	 * @param precision The precision for the new  structure.
	 * @param scale The scale for the new  structure.
	 * @return A new  structure whose Value has been adjusted to the precision and
	 * scale indicated in the parameters.
	 */
	static function ConvertToPrecScale(n:cs.system.data.sqltypes.SqlDecimal, precision:Int, scale:Int):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * The division operator calculates the results of dividing the first  operand by
	 * the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the results of the division.
	 */
	static function Divide(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Performs a logical comparison of the two  operands to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * the value of the  will be null.
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Rounds a specified  number to the next lower whole number.
	 * @param n The  structure for which the floor value is to be calculated.
	 * @return A  structure that contains the whole number part of this  structure.
	 */
	static function Floor(n:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet A .
	 * @return A  value that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	/**
	 * Performs a logical comparison of two  structures to determine whether the first
	 * is greater than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThan(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is greater than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of two  structures to determine whether the first
	 * is less than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The multiplication operator computes the product of the two  parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the product of the
	 * multiplication.
	 */
	static function Multiply(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Calculates the sum of the two  operators.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the sum.
	 */
	static function op_Addition(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * The division operator calculates the results of dividing the first  operand by
	 * the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the results of the division.
	 */
	static function op_Division(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Performs a logical comparison of the two  operands to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are equal or  if the two instances are
	 * not equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlDecimal {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDecimal):cs.system.Decimal {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlDecimal {})
	@:overload(function(x:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlDecimal {})
	@:overload(function(x:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlDecimal {})
	/**
	 * Converts the supplied  structure to .
	 * @param x The  structure to be converted.
	 * @return A new  structure whose  is equal to the  of the  parameter.
	 */
	static function op_Explicit(x:Float):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Performs a logical comparison of two  structures to determine whether the first
	 * is greater than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is greater than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlDecimal {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlDecimal {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt32):cs.system.data.sqltypes.SqlDecimal {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt64):cs.system.data.sqltypes.SqlDecimal {})
	@:overload(function(x:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlDecimal {})
	@:overload(function(x:cs.system.Decimal):cs.system.data.sqltypes.SqlDecimal {})
	/**
	 * Converts the supplied  structure to .
	 * @param x The  structure to be converted.
	 * @return A new  structure whose  property equals the  property of the  parameter.
	 */
	static function op_Implicit(x:haxe.Int64):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of two  structures to determine whether the first
	 * is less than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The multiplication operator computes the product of the two  parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  property contains the product of the
	 * multiplication.
	 */
	static function op_Multiply(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Calculates the results of subtracting the second  operand from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose Value property contains the results of the
	 * subtraction.
	 */
	static function op_Subtraction(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * The unary minus operator negates the  parameter.
	 * @param x The  structure to be negated.
	 * @return A new  structure whose value contains the results of the negation.
	 */
	static function op_UnaryNegation(x:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Converts the  representation of a number to its  equivalent.
	 * @param s The  to be parsed.
	 * @return A  equivalent to the value that is contained in the specified .
	 */
	static function Parse(s:String):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Raises the value of the specified  structure to the specified exponential power.
	 * @param n The  structure to be raised to a power.
	 * @param exp A double value that indicates the power to which the number should be
	 * raised.
	 * @return A  structure that contains the results.
	 */
	static function Power(n:cs.system.data.sqltypes.SqlDecimal, exp:Float):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Gets the number nearest the specified  structure's value with the specified
	 * precision.
	 * @param n The  structure to be rounded.
	 * @param position The number of significant fractional digits (precision) in the
	 * return value.
	 * @return A  structure that contains the results of the rounding operation.
	 */
	static function Round(n:cs.system.data.sqltypes.SqlDecimal, position:Int):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Gets a value that indicates the sign of a  structure's  property.
	 * @param n The  structure whose sign is to be evaluated.
	 * @return A number that indicates the sign of the  structure.
	 */
	static function Sign(n:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlInt32;
	/**
	 * Calculates the results of subtracting the second  operand from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose Value property contains the results of the
	 * subtraction.
	 */
	static function Subtract(x:cs.system.data.sqltypes.SqlDecimal, y:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Truncates the specified  structure's value to the that you want position.
	 * @param n The  structure to be truncated.
	 * @param position The decimal position to which the number will be truncated.
	 * @return Supply a negative value for the  parameter in order to truncate the
	 * value to the corresponding position to the left of the decimal point.
	 */
	static function Truncate(n:cs.system.data.sqltypes.SqlDecimal, position:Int):cs.system.data.sqltypes.SqlDecimal;
	@:overload(function(value:cs.system.data.sqltypes.SqlDecimal):Int {})
	/**
	 * Compares this  instance to the supplied  object and returns an indication of
	 * their relative values.
	 * @param value The  to be compared.
	 * @return A signed number that indicates the relative values of the instance and
	 * the object. Return value Condition Less than zero This instance is less than the
	 * object. Zero This instance is the same as the object. Greater than zero This
	 * instance is greater than the object -or- The object is a null reference ( in
	 * Visual Basic)
	 */
	function CompareTo(value:Dynamic):Int;
	/**
	 * Performs a logical comparison of the two  operands to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * the value of the  will be null.
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the a double equal to the contents of the  property of this instance.
	 * @return The decimal representation of the  property.
	 */
	function ToDouble():Float;
	/**
	 * Converts this  structure to .
	 * @return if the  is non-zero;  if zero; otherwise Null.
	 */
	function ToSqlBoolean():cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts this  structure to .
	 * @return A  structure whose  equals the  of this  structure. If the  structure's
	 * Value is , the  structure's  will be 1. Otherwise, the  structure's  will be 0.
	 */
	function ToSqlByte():cs.system.data.sqltypes.SqlByte;
	/**
	 * Converts this  structure to .
	 * @return A  structure with the same value as this instance of .
	 */
	function ToSqlDouble():cs.system.data.sqltypes.SqlDouble;
	/**
	 * Converts this  structure to .
	 * @return A  structure with the same value as this instance of .
	 */
	function ToSqlInt16():cs.system.data.sqltypes.SqlInt16;
	/**
	 * Converts this  structure to .
	 * @return A  structure with the same value as this instance of .
	 */
	function ToSqlInt32():cs.system.data.sqltypes.SqlInt32;
	/**
	 * Converts this  structure to .
	 * @return A  structure with the same value as this instance of .
	 */
	function ToSqlInt64():cs.system.data.sqltypes.SqlInt64;
	/**
	 * Converts this  structure to .
	 * @return A  structure with the same value as this instance of .
	 */
	function ToSqlMoney():cs.system.data.sqltypes.SqlMoney;
	/**
	 * Converts this  structure to .
	 * @return A  structure with the same value as this instance of .
	 */
	function ToSqlSingle():cs.system.data.sqltypes.SqlSingle;
	/**
	 * Converts this  structure to .
	 * @return A  structure whose value is a string representing the value contained in
	 * this  structure.
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Converts this  structure to .
	 * @return A new  object that contains the string representation of the 
	 * structure's  property.
	 */
	function ToString():String;
}
