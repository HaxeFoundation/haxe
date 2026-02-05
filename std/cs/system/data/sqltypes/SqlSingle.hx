package cs.system.data.sqltypes;

/** Represents a floating point number within the range of -3.40E +38 through 3.40E +38 to be stored in or retrieved from a database. */
@:native("System.Data.SqlTypes.SqlSingle")
extern class SqlSingle extends cs.system.ValueType {
	/** Represents the maximum value that can be assigned to the  property of an instance of the  class. */
	static var MaxValue(default, never):cs.system.data.sqltypes.SqlSingle;
	/** Represents the minimum value that can be assigned to  property of an instance of the  class. */
	static var MinValue(default, never):cs.system.data.sqltypes.SqlSingle;
	/** Represents a  that can be assigned to this instance of the  structure. */
	static var Null(default, never):cs.system.data.sqltypes.SqlSingle;
	/** Represents the zero value that can be assigned to the  property of an instance of the  class. */
	static var Zero(default, never):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Indicates whether this  structure is null.
	 * @return if null. Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the value of this  structure. This property is read-only.
	 * @return A floating point value in the range -3.40E+38 through 3.40E+38.
	 */
	var Value(default, never):Single;
	@:overload(function(value:Float):Void {})
	function new(value:Single):Void;
	/**
	 * Computes the sum of the two specified  structures.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the sum of the two specified  structures.
	 */
	static function Add(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Divides the first  structure by the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the results of the division.
	 */
	static function Divide(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet A .
	 * @return A  value that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is greater than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThan(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of two  structures to determine whether the first
	 * is greater than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Computes the product of the two specified  structures.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the product of the multiplication.
	 */
	static function Multiply(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Computes the sum of the two specified  structures.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the sum of the two specified  structures.
	 */
	static function op_Addition(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Divides the first  structure by the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the results of the division.
	 */
	static function op_Division(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Performs a logical comparison of the two SqlSingle parameters to determine
	 * whether they are equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are equal or  if the two instances are
	 * not equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlSingle {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlSingle {})
	@:overload(function(x:cs.system.data.sqltypes.SqlSingle):Single {})
	/**
	 * This implicit operator converts the supplied  to .
	 * @param x The  structure to be converted.
	 * @return A new  structure whose  is equal to the  of the  parameter.
	 */
	static function op_Explicit(x:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Performs a logical comparison of the two  operands to determine whether the
	 * first is greater than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of two  structures to determine whether the first
	 * is greater than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlSingle {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlSingle {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlSingle {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt32):cs.system.data.sqltypes.SqlSingle {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt64):cs.system.data.sqltypes.SqlSingle {})
	@:overload(function(x:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlSingle {})
	/**
	 * This implicit operator converts the  parameter to .
	 * @param x The  to be converted.
	 * @return A new  structure whose  property equals the  of the  parameter.
	 */
	static function op_Implicit(x:Single):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Computes the product of the two specified  structures.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the product of the multiplication.
	 */
	static function op_Multiply(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Subtracts the second  structure from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the results of the subtraction.
	 */
	static function op_Subtraction(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Negates the  of the specified  structure.
	 * @param x A  structure.
	 * @return A  structure that contains the negated value.
	 */
	static function op_UnaryNegation(x:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Converts the specified  to a  structure.
	 * @param s The  to be parsed.
	 * @return A  equivalent to the value that is contained in the specified .
	 */
	static function Parse(s:String):cs.system.data.sqltypes.SqlSingle;
	/**
	 * Subtracts the second  structure from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  structure that contains the results of the subtraction.
	 */
	static function Subtract(x:cs.system.data.sqltypes.SqlSingle, y:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlSingle;
	@:overload(function(value:cs.system.data.sqltypes.SqlSingle):Int {})
	/**
	 * Compares this  instance to the supplied  and returns an indication of their
	 * relative values.
	 * @param value The  to be compared.
	 * @return A signed number that indicates the relative values of the instance and
	 * the object. Return Value Condition Less than zero This instance is less than the
	 * object. Zero This instance is the same as the object. Greater than zero This
	 * instance is greater than the object -or- The object is a null reference ( in
	 * Visual Basic)
	 */
	function CompareTo(value:Dynamic):Int;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are equal.
	 * @param x A  structure.
	 * @param y A  structure.
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
	 * @return A new  equal to the value of this .
	 */
	function ToSqlDecimal():cs.system.data.sqltypes.SqlDecimal;
	/**
	 * Converts this  structure to .
	 * @return A new  equal to the value of this .
	 */
	function ToSqlDouble():cs.system.data.sqltypes.SqlDouble;
	/**
	 * Converts this  structure to .
	 * @return A new  equal to the value of this .
	 */
	function ToSqlInt16():cs.system.data.sqltypes.SqlInt16;
	/**
	 * Converts this  structure to .
	 * @return A new  equal to the value of this .
	 */
	function ToSqlInt32():cs.system.data.sqltypes.SqlInt32;
	/**
	 * Converts this  structure to .
	 * @return A new  equal to the value of this .
	 */
	function ToSqlInt64():cs.system.data.sqltypes.SqlInt64;
	/**
	 * Converts this  structure to .
	 * @return A  equal to the value of this .
	 */
	function ToSqlMoney():cs.system.data.sqltypes.SqlMoney;
	/**
	 * Converts this  structure to .
	 * @return A  representing the value of this .
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Converts this  structure to .
	 * @return A  object representing the value of this .
	 */
	function ToString():String;
}
