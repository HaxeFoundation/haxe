package cs.system;

/** Represents a time interval. */
@:native("System.TimeSpan")
extern class TimeSpan extends cs.system.ValueType {
	/** Represents the maximum  value. This field is read-only. */
	static var MaxValue(default, never):cs.system.TimeSpan;
	/** Represents the minimum  value. This field is read-only. */
	static var MinValue(default, never):cs.system.TimeSpan;
	/** Represents the number of ticks in 1 day. This field is constant. */
	static var TicksPerDay(default, never):haxe.Int64;
	/** Represents the number of ticks in 1 hour. This field is constant. */
	static var TicksPerHour(default, never):haxe.Int64;
	/** Represents the number of ticks in 1 millisecond. This field is constant. */
	static var TicksPerMillisecond(default, never):haxe.Int64;
	/** Represents the number of ticks in 1 minute. This field is constant. */
	static var TicksPerMinute(default, never):haxe.Int64;
	/** Represents the number of ticks in 1 second. */
	static var TicksPerSecond(default, never):haxe.Int64;
	/** Represents the zero  value. This field is read-only. */
	static var Zero(default, never):cs.system.TimeSpan;
	/**
	 * Gets the days component of the time interval represented by the current 
	 * structure.
	 * @return The day component of this instance. The return value can be positive or
	 * negative.
	 */
	var Days(default, never):Int;
	/**
	 * Gets the hours component of the time interval represented by the current 
	 * structure.
	 * @return The hour component of the current  structure. The return value ranges
	 * from -23 through 23.
	 */
	var Hours(default, never):Int;
	/**
	 * Gets the milliseconds component of the time interval represented by the current 
	 * structure.
	 * @return The millisecond component of the current  structure. The return value
	 * ranges from -999 through 999.
	 */
	var Milliseconds(default, never):Int;
	/**
	 * Gets the minutes component of the time interval represented by the current 
	 * structure.
	 * @return The minute component of the current  structure. The return value ranges
	 * from -59 through 59.
	 */
	var Minutes(default, never):Int;
	/**
	 * Gets the seconds component of the time interval represented by the current 
	 * structure.
	 * @return The second component of the current  structure. The return value ranges
	 * from -59 through 59.
	 */
	var Seconds(default, never):Int;
	/**
	 * Gets the number of ticks that represent the value of the current  structure.
	 * @return The number of ticks contained in this instance.
	 */
	var Ticks(default, never):haxe.Int64;
	/**
	 * Gets the value of the current  structure expressed in whole and fractional days.
	 * @return The total number of days represented by this instance.
	 */
	var TotalDays(default, never):Float;
	/**
	 * Gets the value of the current  structure expressed in whole and fractional
	 * hours.
	 * @return The total number of hours represented by this instance.
	 */
	var TotalHours(default, never):Float;
	/**
	 * Gets the value of the current  structure expressed in whole and fractional
	 * milliseconds.
	 * @return The total number of milliseconds represented by this instance.
	 */
	var TotalMilliseconds(default, never):Float;
	/**
	 * Gets the value of the current  structure expressed in whole and fractional
	 * minutes.
	 * @return The total number of minutes represented by this instance.
	 */
	var TotalMinutes(default, never):Float;
	/**
	 * Gets the value of the current  structure expressed in whole and fractional
	 * seconds.
	 * @return The total number of seconds represented by this instance.
	 */
	var TotalSeconds(default, never):Float;
	@:overload(function(ticks:haxe.Int64):Void {})
	@:overload(function(hours:Int, minutes:Int, seconds:Int):Void {})
	@:overload(function(days:Int, hours:Int, minutes:Int, seconds:Int):Void {})
	function new(days:Int, hours:Int, minutes:Int, seconds:Int, milliseconds:Int):Void;
	/**
	 * Compares two  values and returns an integer that indicates whether the first
	 * value is shorter than, equal to, or longer than the second value.
	 * @param t1 The first time interval to compare.
	 * @param t2 The second time interval to compare.
	 * @return One of the following values. Value Description -1 is shorter than . 0 is
	 * equal to . 1 is longer than .
	 */
	static function Compare(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):Int;
	/**
	 * Returns a value indicating whether this instance is equal to a specified object.
	 * @param value An object to compare with this instance.
	 * @return if  is a  object that represents the same time interval as the current 
	 * structure; otherwise, .
	 */
	static function Equals(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):Bool;
	/**
	 * Returns a  that represents a specified number of days, where the specification
	 * is accurate to the nearest millisecond.
	 * @param value A number of days, accurate to the nearest millisecond.
	 * @return An object that represents .
	 */
	static function FromDays(value:Float):cs.system.TimeSpan;
	/**
	 * Returns a  that represents a specified number of hours, where the specification
	 * is accurate to the nearest millisecond.
	 * @param value A number of hours accurate to the nearest millisecond.
	 * @return An object that represents .
	 */
	static function FromHours(value:Float):cs.system.TimeSpan;
	/**
	 * Returns a  that represents a specified number of milliseconds.
	 * @param value A number of milliseconds.
	 * @return An object that represents .
	 */
	static function FromMilliseconds(value:Float):cs.system.TimeSpan;
	/**
	 * Returns a  that represents a specified number of minutes, where the
	 * specification is accurate to the nearest millisecond.
	 * @param value A number of minutes, accurate to the nearest millisecond.
	 * @return An object that represents .
	 */
	static function FromMinutes(value:Float):cs.system.TimeSpan;
	/**
	 * Returns a  that represents a specified number of seconds, where the
	 * specification is accurate to the nearest millisecond.
	 * @param value A number of seconds, accurate to the nearest millisecond.
	 * @return An object that represents .
	 */
	static function FromSeconds(value:Float):cs.system.TimeSpan;
	/**
	 * Returns a  that represents a specified time, where the specification is in units
	 * of ticks.
	 * @param value A number of ticks that represent a time.
	 * @return An object that represents .
	 */
	static function FromTicks(value:haxe.Int64):cs.system.TimeSpan;
	/**
	 * Adds two specified  instances.
	 * @param t1 The first time interval to add.
	 * @param t2 The second time interval to add.
	 * @return An object whose value is the sum of the values of  and .
	 */
	static function op_Addition(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):cs.system.TimeSpan;
	@:overload(function(timeSpan:cs.system.TimeSpan, divisor:Float):cs.system.TimeSpan {})
	/**
	 * @param timeSpan 
	 * @param divisor 
	 */
	static function op_Division(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):Float;
	/**
	 * Indicates whether two  instances are equal.
	 * @param t1 The first time interval to compare.
	 * @param t2 The second time interval to compare.
	 * @return if the values of  and  are equal; otherwise, .
	 */
	static function op_Equality(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):Bool;
	/**
	 * Indicates whether a specified  is greater than another specified .
	 * @param t1 The first time interval to compare.
	 * @param t2 The second time interval to compare.
	 * @return if the value of  is greater than the value of ; otherwise, .
	 */
	static function op_GreaterThan(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):Bool;
	/**
	 * Indicates whether a specified  is greater than or equal to another specified .
	 * @param t1 The first time interval to compare.
	 * @param t2 The second time interval to compare.
	 * @return if the value of  is greater than or equal to the value of ; otherwise, .
	 */
	static function op_GreaterThanOrEqual(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):Bool;
	/**
	 * Indicates whether two  instances are not equal.
	 * @param t1 The first time interval to compare.
	 * @param t2 The second time interval to compare.
	 * @return if the values of  and  are not equal; otherwise, .
	 */
	static function op_Inequality(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):Bool;
	/**
	 * Indicates whether a specified  is less than another specified .
	 * @param t1 The first time interval to compare.
	 * @param t2 The second time interval to compare.
	 * @return if the value of  is less than the value of ; otherwise, .
	 */
	static function op_LessThan(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):Bool;
	/**
	 * Indicates whether a specified  is less than or equal to another specified .
	 * @param t1 The first time interval to compare.
	 * @param t2 The second time interval to compare.
	 * @return if the value of  is less than or equal to the value of ; otherwise, .
	 */
	static function op_LessThanOrEqual(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):Bool;
	@:overload(function(factor:Float, timeSpan:cs.system.TimeSpan):cs.system.TimeSpan {})
	/**
	 * @param factor 
	 * @param timeSpan 
	 */
	static function op_Multiply(timeSpan:cs.system.TimeSpan, factor:Float):cs.system.TimeSpan;
	/**
	 * Subtracts a specified  from another specified .
	 * @param t1 The minuend.
	 * @param t2 The subtrahend.
	 * @return An object whose value is the result of the value of  minus the value of
	 * .
	 */
	static function op_Subtraction(t1:cs.system.TimeSpan, t2:cs.system.TimeSpan):cs.system.TimeSpan;
	/**
	 * Returns a  whose value is the negated value of the specified instance.
	 * @param t The time interval to be negated.
	 * @return An object that has the same numeric value as this instance, but the
	 * opposite sign.
	 */
	static function op_UnaryNegation(t:cs.system.TimeSpan):cs.system.TimeSpan;
	/**
	 * Returns the specified instance of .
	 * @param t The time interval to return.
	 * @return The time interval specified by .
	 */
	static function op_UnaryPlus(t:cs.system.TimeSpan):cs.system.TimeSpan;
	@:overload(function(s:String):cs.system.TimeSpan {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, ?formatProvider:cs.system.IFormatProvider):cs.system.TimeSpan {})
	/**
	 * @param input 
	 * @param formatProvider 
	 */
	static function Parse(input:String, formatProvider:cs.system.IFormatProvider):cs.system.TimeSpan;
	@:overload(function(input:String, format:String, formatProvider:cs.system.IFormatProvider):cs.system.TimeSpan {})
	@:overload(function(input:String, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider):cs.system.TimeSpan {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, format:cs.system.ReadOnlySpan<cs.Char16>, formatProvider:cs.system.IFormatProvider, ?styles:cs.system.globalization.TimeSpanStyles):cs.system.TimeSpan {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, ?styles:cs.system.globalization.TimeSpanStyles):cs.system.TimeSpan {})
	@:overload(function(input:String, format:String, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.TimeSpanStyles):cs.system.TimeSpan {})
	/**
	 * @param input 
	 * @param format 
	 * @param formatProvider 
	 * @param styles 
	 */
	static function ParseExact(input:String, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.TimeSpanStyles):cs.system.TimeSpan;
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	@:overload(function(s:String, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, formatProvider:cs.system.IFormatProvider, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	/**
	 * @param input 
	 * @param formatProvider 
	 * @param result 
	 */
	static function TryParse(input:String, formatProvider:cs.system.IFormatProvider, result:cs.Ref<cs.system.TimeSpan>):Bool;
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, format:cs.system.ReadOnlySpan<cs.Char16>, formatProvider:cs.system.IFormatProvider, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	@:overload(function(input:String, format:String, formatProvider:cs.system.IFormatProvider, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	@:overload(function(input:String, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, format:cs.system.ReadOnlySpan<cs.Char16>, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.TimeSpanStyles, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.TimeSpanStyles, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	@:overload(function(input:String, format:String, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.TimeSpanStyles, result:cs.Ref<cs.system.TimeSpan>):Bool {})
	/**
	 * @param input 
	 * @param format 
	 * @param formatProvider 
	 * @param styles 
	 * @param result 
	 */
	static function TryParseExact(input:String, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.TimeSpanStyles, result:cs.Ref<cs.system.TimeSpan>):Bool;
	/**
	 * Returns a new  object whose value is the sum of the specified  object and this
	 * instance.
	 * @param ts The time interval to add.
	 * @return A new object that represents the value of this instance plus the value
	 * of .
	 */
	function Add(ts:cs.system.TimeSpan):cs.system.TimeSpan;
	@:overload(function(value:Dynamic):Int {})
	/**
	 * Compares this instance to a specified object and returns an integer that
	 * indicates whether this instance is shorter than, equal to, or longer than the
	 * specified object.
	 * @param value An object to compare, or .
	 * @return One of the following values. Value Description -1 This instance is
	 * shorter than . 0 This instance is equal to . 1 This instance is longer than .
	 * -or- is .
	 */
	function CompareTo(value:cs.system.TimeSpan):Int;
	@:overload(function(divisor:Float):cs.system.TimeSpan {})
	/** @param divisor  */
	function Divide(ts:cs.system.TimeSpan):Float;
	/**
	 * Returns a new  object whose value is the absolute value of the current  object.
	 * @return A new object whose value is the absolute value of the current  object.
	 */
	function Duration():cs.system.TimeSpan;
	@:overload(function(value:Dynamic):Bool {})
	/**
	 * Returns a value indicating whether this instance is equal to a specified object.
	 * @param value An object to compare with this instance.
	 * @return if  is a  object that represents the same time interval as the current 
	 * structure; otherwise, .
	 */
	function Equals(obj:cs.system.TimeSpan):Bool;
	/**
	 * Returns a hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/** @param factor  */
	function Multiply(factor:Float):cs.system.TimeSpan;
	/**
	 * Returns a new  object whose value is the negated value of this instance.
	 * @return A new object with the same numeric value as this instance, but with the
	 * opposite sign.
	 */
	function Negate():cs.system.TimeSpan;
	/**
	 * Returns a new  object whose value is the difference between the specified 
	 * object and this instance.
	 * @param ts The time interval to be subtracted.
	 * @return A new time interval whose value is the result of the value of this
	 * instance minus the value of .
	 */
	function Subtract(ts:cs.system.TimeSpan):cs.system.TimeSpan;
	@:overload(function():String {})
	@:overload(function(format:String):String {})
	/**
	 * Converts the value of the current  object to its equivalent string
	 * representation.
	 * @return The string representation of the current  value.
	 */
	function ToString(format:String, formatProvider:cs.system.IFormatProvider):String;
	/**
	 * @param destination 
	 * @param charsWritten 
	 * @param format 
	 * @param formatProvider 
	 */
	function TryFormat(destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>, ?format:cs.system.ReadOnlySpan<cs.Char16>, ?formatProvider:cs.system.IFormatProvider):Bool;
}
