package cs.system.data.sqltypes;

/** Represents the date and time data ranging in value from January 1, 1753 to December 31, 9999 to an accuracy of 3.33 milliseconds to be stored in or retrieved from a database. The  structure has a different underlying data structure from its corresponding .NET Framework type, , which can represent any time between 12:00:00 AM 1/1/0001 and 11:59:59 PM 12/31/9999, to the accuracy of 100 nanoseconds.  actually stores the relative difference to 00:00:00 AM 1/1/1900. Therefore, a conversion from "00:00:00 AM 1/1/1900" to an integer will return 0. */
@:native("System.Data.SqlTypes.SqlDateTime")
extern class SqlDateTime extends cs.system.ValueType {
	/** Represents the maximum valid date value for a  structure. */
	static var MaxValue(default, never):cs.system.data.sqltypes.SqlDateTime;
	/** Represents the minimum valid date value for a  structure. */
	static var MinValue(default, never):cs.system.data.sqltypes.SqlDateTime;
	/** Represents a  that can be assigned to this instance of the  structure. */
	static var Null(default, never):cs.system.data.sqltypes.SqlDateTime;
	/** A constant whose value is the number of ticks equivalent to one hour. */
	static var SQLTicksPerHour(default, never):Int;
	/** A constant whose value is the number of ticks equivalent to one minute. */
	static var SQLTicksPerMinute(default, never):Int;
	/** A constant whose value is the number of ticks equivalent to one second. */
	static var SQLTicksPerSecond(default, never):Int;
	/**
	 * Gets the number of ticks representing the date of this  structure.
	 * @return The number of ticks representing the date that is contained in the 
	 * property of this  structure.
	 */
	var DayTicks(default, never):Int;
	/**
	 * Indicates whether this  structure is null.
	 * @return if null. Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the number of ticks representing the time of this  structure.
	 * @return The number of ticks representing the time of this  structure.
	 */
	var TimeTicks(default, never):Int;
	/**
	 * Gets the value of the  structure. This property is read-only.
	 * @return The value of this  structure.
	 */
	var Value(default, never):cs.system.DateTime;
	@:overload(function(value:cs.system.DateTime):Void {})
	@:overload(function(dayTicks:Int, timeTicks:Int):Void {})
	@:overload(function(year:Int, month:Int, day:Int):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Float):Void {})
	function new(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, bilisecond:Int):Void;
	/**
	 * Adds a  to the specified .
	 * @param x A  value.
	 * @param t A  value.
	 * @return A  value.
	 */
	static function Add(x:cs.system.data.sqltypes.SqlDateTime, t:cs.system.TimeSpan):cs.system.data.sqltypes.SqlDateTime;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, .
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
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
	static function GreaterThan(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is greater than or
	 * equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of two instances of  to determine whether they are
	 * not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Adds the period of time indicated by the supplied  parameter, , to the supplied 
	 * structure.
	 * @param x A  structure.
	 * @param t A  structure.
	 * @return A new . If either argument is , the new  is .
	 */
	static function op_Addition(x:cs.system.data.sqltypes.SqlDateTime, t:cs.system.TimeSpan):cs.system.data.sqltypes.SqlDateTime;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlDateTime):cs.system.DateTime {})
	/**
	 * Converts the  structure to a  structure.
	 * @param x A  structure.
	 * @return A  object whose  and  properties contain the same date and time values
	 * as the  property of the supplied  structure.
	 */
	static function op_Explicit(x:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlDateTime;
	/**
	 * Compares two instances of  to determine whether the first is greater than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is greater than or
	 * equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts a  structure to a  structure.
	 * @param value A  structure.
	 * @return A  structure whose  is equal to the combined  and  properties of the
	 * supplied  structure.
	 */
	static function op_Implicit(value:cs.system.DateTime):cs.system.data.sqltypes.SqlDateTime;
	/**
	 * Performs a logical comparison of two instances of  to determine whether they are
	 * not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlDateTime, y:cs.system.data.sqltypes.SqlDateTime):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Subtracts the supplied  structure, , from the supplied  structure.
	 * @param x A  structure.
	 * @param t A  structure.
	 * @return A  structure representing the results of the subtraction.
	 */
	static function op_Subtraction(x:cs.system.data.sqltypes.SqlDateTime, t:cs.system.TimeSpan):cs.system.data.sqltypes.SqlDateTime;
	/**
	 * Converts the specified  representation of a date and time to its  equivalent.
	 * @param s The  to be parsed.
	 * @return A  structure equal to the date and time represented by the specified .
	 */
	static function Parse(s:String):cs.system.data.sqltypes.SqlDateTime;
	/**
	 * Subtracts the specified  from this  instance.
	 * @param x A  value.
	 * @param t A  value.
	 * @return A  value.
	 */
	static function Subtract(x:cs.system.data.sqltypes.SqlDateTime, t:cs.system.TimeSpan):cs.system.data.sqltypes.SqlDateTime;
	@:overload(function(value:cs.system.data.sqltypes.SqlDateTime):Int {})
	/**
	 * Compares this  structure to the supplied  structure and returns an indication of
	 * their relative values.
	 * @param value The  structure to be compared.
	 * @return A signed number that indicates the relative values of the instance and
	 * the object. Return value Condition Less than zero This instance is less than .
	 * Zero This instance is the same as . Greater than zero This instance is greater
	 * than -or- is a null reference ( in Visual Basic)
	 */
	function CompareTo(value:Dynamic):Int;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Gets the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Converts this  structure to .
	 * @return A  structure whose value is a string representing the date and time that
	 * is contained in this  structure.
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Converts this  structure to a .
	 * @return A  representing the  property of this  structure.
	 */
	function ToString():String;
}
