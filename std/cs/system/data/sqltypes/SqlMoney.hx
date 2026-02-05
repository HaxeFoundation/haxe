package cs.system.data.sqltypes;

/** Represents a currency value ranging from -2 63 (or -922,337,203,685,477.5808) to 2 63 -1 (or +922,337,203,685,477.5807) with an accuracy to a ten-thousandth of currency unit to be stored in or retrieved from a database. */
@:native("System.Data.SqlTypes.SqlMoney")
extern class SqlMoney extends cs.system.ValueType {
	/** Represents the maximum value that can be assigned to the  property of an instance of the  class. */
	static var MaxValue(default, never):cs.system.data.sqltypes.SqlMoney;
	/** Represents the minimum value that can be assigned to  property of an instance of the  class. */
	static var MinValue(default, never):cs.system.data.sqltypes.SqlMoney;
	/** Represents a  that can be assigned to this instance of the  class. */
	static var Null(default, never):cs.system.data.sqltypes.SqlMoney;
	/** Represents the zero value that can be assigned to the  property of an instance of the  class. */
	static var Zero(default, never):cs.system.data.sqltypes.SqlMoney;
	/**
	 * Returns a Boolean value that indicates whether this  structure is null.
	 * @return if null. Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the monetary value of an instance of the  structure. This property is
	 * read-only.
	 * @return The monetary value of an instance of the  structure.
	 */
	var Value(default, never):cs.system.Decimal;
	@:overload(function(value:cs.system.Decimal):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	function new(value:haxe.Int64):Void;
	/**
	 * Calculates the sum of the two  parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  contains the sum of the two  parameters.
	 */
	static function Add(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlMoney;
	/**
	 * The division operator divides the first  parameter by the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  contains the results of the division.
	 */
	static function Divide(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlMoney;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet An .
	 * @return A  that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is greater than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThan(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is greater than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The multiplication operator calculates the product of the two  parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  contains the product of the multiplication.
	 */
	static function Multiply(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlMoney;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Calculates the sum of the two  parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  contains the sum of the two  parameters.
	 */
	static function op_Addition(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlMoney;
	/**
	 * The division operator divides the first  parameter by the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  contains the results of the division.
	 */
	static function op_Division(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlMoney;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are equal or  if the two instances are
	 * not equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlBoolean):cs.system.data.sqltypes.SqlMoney {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDecimal):cs.system.data.sqltypes.SqlMoney {})
	@:overload(function(x:cs.system.data.sqltypes.SqlDouble):cs.system.data.sqltypes.SqlMoney {})
	@:overload(function(x:cs.system.data.sqltypes.SqlMoney):cs.system.Decimal {})
	@:overload(function(x:cs.system.data.sqltypes.SqlSingle):cs.system.data.sqltypes.SqlMoney {})
	@:overload(function(x:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlMoney {})
	/**
	 * This implicit operator converts the supplied  parameter to .
	 * @param x The  structure to be converted.
	 * @return A new  structure whose  property equals the  property of the  parameter.
	 */
	static function op_Explicit(x:Float):cs.system.data.sqltypes.SqlMoney;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is greater than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is greater than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlByte):cs.system.data.sqltypes.SqlMoney {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt16):cs.system.data.sqltypes.SqlMoney {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt32):cs.system.data.sqltypes.SqlMoney {})
	@:overload(function(x:cs.system.data.sqltypes.SqlInt64):cs.system.data.sqltypes.SqlMoney {})
	@:overload(function(x:cs.system.Decimal):cs.system.data.sqltypes.SqlMoney {})
	/**
	 * This implicit operator converts the supplied  parameter to .
	 * @param x The  structure to be converted.
	 * @return A new  structure whose  property is equal to the  of the  parameter.
	 */
	static function op_Implicit(x:haxe.Int64):cs.system.data.sqltypes.SqlMoney;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether they
	 * are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of the two  parameters to determine whether the
	 * first is less than or equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * The multiplication operator calculates the product of the two  parameters.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure whose  contains the product of the multiplication.
	 */
	static function op_Multiply(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlMoney;
	/**
	 * The subtraction operator subtracts the second  parameter from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure that contains the results of the subtraction.
	 */
	static function op_Subtraction(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlMoney;
	/**
	 * The unary minus operator negates the  parameter.
	 * @param x The  structure to be negated.
	 * @return A  structure whose  contains the results of the negation.
	 */
	static function op_UnaryNegation(x:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlMoney;
	/**
	 * Converts the  representation of a number to its  equivalent.
	 * @param s The  to be parsed.
	 * @return A  equivalent to the value that is contained in the specified .
	 */
	static function Parse(s:String):cs.system.data.sqltypes.SqlMoney;
	/**
	 * The subtraction operator subtracts the second  parameter from the first.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A new  structure that contains the results of the subtraction.
	 */
	static function Subtract(x:cs.system.data.sqltypes.SqlMoney, y:cs.system.data.sqltypes.SqlMoney):cs.system.data.sqltypes.SqlMoney;
	@:overload(function(value:cs.system.data.sqltypes.SqlMoney):Int {})
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
	 * Converts the Value of this instance of  as a  structure.
	 * @return A  structure whose value equals the  property of this  structure.
	 */
	function ToDecimal():cs.system.Decimal;
	/**
	 * Converts this  structure to a .
	 * @return A double with a value equal to this  structure.
	 */
	function ToDouble():Float;
	/**
	 * Converts this  structure to an .
	 * @return A 32-bit integer whose value equals the integer part of this  structure.
	 */
	function ToInt32():Int;
	/**
	 * Converts the Value of this  structure to an .
	 * @return A 64-bit integer whose value equals the integer part of this  structure.
	 */
	function ToInt64():haxe.Int64;
	/**
	 * Converts this  structure to .
	 * @return A  structure. If the value of the  structure is zero, the  structure's
	 * value will be . Otherwise, .
	 */
	function ToSqlBoolean():cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts this  structure to .
	 * @return A  equal to the value of this .
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
	 * @return A new  equal to the value of this .
	 */
	function ToSqlSingle():cs.system.data.sqltypes.SqlSingle;
	/**
	 * Converts this  structure to .
	 * @return A  structure whose value is a string representing the value of this .
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Converts this instance of  to string.
	 * @return A string whose value is the string representation of the value of this .
	 */
	function ToString():String;
}
