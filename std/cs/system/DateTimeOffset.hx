package cs.system;

/** Represents a point in time, typically expressed as a date and time of day, relative to Coordinated Universal Time (UTC). */
@:native("System.DateTimeOffset")
extern class DateTimeOffset extends cs.system.ValueType {
	/** Represents the greatest possible value of . This field is read-only. */
	static var MaxValue(default, never):cs.system.DateTimeOffset;
	/** Represents the earliest possible  value. This field is read-only. */
	static var MinValue(default, never):cs.system.DateTimeOffset;
	static var UnixEpoch(default, never):cs.system.DateTimeOffset;
	/**
	 * Gets a  object that is set to the current date and time on the current computer,
	 * with the offset set to the local time's offset from Coordinated Universal Time
	 * (UTC).
	 * @return A  object whose date and time is the current local time and whose offset
	 * is the local time zone's offset from Coordinated Universal Time (UTC).
	 */
	static var Now(default, never):cs.system.DateTimeOffset;
	/**
	 * Gets a  object whose date and time are set to the current Coordinated Universal
	 * Time (UTC) date and time and whose offset is .
	 * @return An object whose date and time is the current Coordinated Universal Time
	 * (UTC) and whose offset is .
	 */
	static var UtcNow(default, never):cs.system.DateTimeOffset;
	/**
	 * Gets a  value that represents the date component of the current  object.
	 * @return A  value that represents the date component of the current  object.
	 */
	var Date(default, never):cs.system.DateTime;
	/**
	 * Gets a  value that represents the date and time of the current  object.
	 * @return The date and time of the current  object.
	 */
	var DateTime(default, never):cs.system.DateTime;
	/**
	 * Gets the day of the month represented by the current  object.
	 * @return The day component of the current  object, expressed as a value between 1
	 * and 31.
	 */
	var Day(default, never):Int;
	/**
	 * Gets the day of the week represented by the current  object.
	 * @return One of the enumeration values that indicates the day of the week of the
	 * current  object.
	 */
	var DayOfWeek(default, never):cs.system.DayOfWeek;
	/**
	 * Gets the day of the year represented by the current  object.
	 * @return The day of the year of the current  object, expressed as a value between
	 * 1 and 366.
	 */
	var DayOfYear(default, never):Int;
	/**
	 * Gets the hour component of the time represented by the current  object.
	 * @return The hour component of the current  object. This property uses a 24-hour
	 * clock; the value ranges from 0 to 23.
	 */
	var Hour(default, never):Int;
	/**
	 * Gets a  value that represents the local date and time of the current  object.
	 * @return The local date and time of the current  object.
	 */
	var LocalDateTime(default, never):cs.system.DateTime;
	/**
	 * Gets the millisecond component of the time represented by the current  object.
	 * @return The millisecond component of the current  object, expressed as an
	 * integer between 0 and 999.
	 */
	var Millisecond(default, never):Int;
	/**
	 * Gets the minute component of the time represented by the current  object.
	 * @return The minute component of the current  object, expressed as an integer
	 * between 0 and 59.
	 */
	var Minute(default, never):Int;
	/**
	 * Gets the month component of the date represented by the current  object.
	 * @return The month component of the current  object, expressed as an integer
	 * between 1 and 12.
	 */
	var Month(default, never):Int;
	/**
	 * Gets the time's offset from Coordinated Universal Time (UTC).
	 * @return The difference between the current  object's time value and Coordinated
	 * Universal Time (UTC).
	 */
	var Offset(default, never):cs.system.TimeSpan;
	/**
	 * Gets the second component of the clock time represented by the current  object.
	 * @return The second component of the  object, expressed as an integer value
	 * between 0 and 59.
	 */
	var Second(default, never):Int;
	/**
	 * Gets the number of ticks that represents the date and time of the current 
	 * object in clock time.
	 * @return The number of ticks in the  object's clock time.
	 */
	var Ticks(default, never):haxe.Int64;
	/**
	 * Gets the time of day for the current  object.
	 * @return The time interval of the current date that has elapsed since midnight.
	 */
	var TimeOfDay(default, never):cs.system.TimeSpan;
	/**
	 * Gets a  value that represents the Coordinated Universal Time (UTC) date and time
	 * of the current  object.
	 * @return The Coordinated Universal Time (UTC) date and time of the current 
	 * object.
	 */
	var UtcDateTime(default, never):cs.system.DateTime;
	/**
	 * Gets the number of ticks that represents the date and time of the current 
	 * object in Coordinated Universal Time (UTC).
	 * @return The number of ticks in the  object's Coordinated Universal Time (UTC).
	 */
	var UtcTicks(default, never):haxe.Int64;
	/**
	 * Gets the year component of the date represented by the current  object.
	 * @return The year component of the current  object, expressed as an integer value
	 * between 0 and 9999.
	 */
	var Year(default, never):Int;
	@:overload(function(dateTime:cs.system.DateTime):Void {})
	@:overload(function(dateTime:cs.system.DateTime, offset:cs.system.TimeSpan):Void {})
	@:overload(function(ticks:haxe.Int64, offset:cs.system.TimeSpan):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, offset:cs.system.TimeSpan):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, offset:cs.system.TimeSpan):Void {})
	function new(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, calendar:cs.system.globalization.Calendar, offset:cs.system.TimeSpan):Void;
	/**
	 * Compares two  objects and indicates whether the first is earlier than the
	 * second, equal to the second, or later than the second.
	 * @param first The first object to compare.
	 * @param second The second object to compare.
	 * @return A signed integer that indicates whether the value of the  parameter is
	 * earlier than, later than, or the same time as the value of the  parameter, as
	 * the following table shows. Return value Meaning Less than zero is earlier than .
	 * Zero is equal to . Greater than zero is later than .
	 */
	static function Compare(first:cs.system.DateTimeOffset, second:cs.system.DateTimeOffset):Int;
	/**
	 * Determines whether the current  object represents the same point in time as a
	 * specified  object.
	 * @param other An object to compare to the current  object.
	 * @return if both  objects have the same  value; otherwise, .
	 */
	static function Equals(first:cs.system.DateTimeOffset, second:cs.system.DateTimeOffset):Bool;
	/**
	 * Converts the specified Windows file time to an equivalent local time.
	 * @param fileTime A Windows file time, expressed in ticks.
	 * @return An object that represents the date and time of  with the offset set to
	 * the local time offset.
	 */
	static function FromFileTime(fileTime:haxe.Int64):cs.system.DateTimeOffset;
	/**
	 * Converts a Unix time expressed as the number of milliseconds that have elapsed
	 * since 1970-01-01T00:00:00Z to a  value.
	 * @param milliseconds A Unix time, expressed as the number of milliseconds that
	 * have elapsed since 1970-01-01T00:00:00Z (January 1, 1970, at 12:00 AM UTC). For
	 * Unix times before this date, its value is negative.
	 * @return A date and time value that represents the same moment in time as the
	 * Unix time.
	 */
	static function FromUnixTimeMilliseconds(milliseconds:haxe.Int64):cs.system.DateTimeOffset;
	/**
	 * Converts a Unix time expressed as the number of seconds that have elapsed since
	 * 1970-01-01T00:00:00Z to a  value.
	 * @param seconds A Unix time, expressed as the number of seconds that have elapsed
	 * since 1970-01-01T00:00:00Z (January 1, 1970, at 12:00 AM UTC). For Unix times
	 * before this date, its value is negative.
	 * @return A date and time value that represents the same moment in time as the
	 * Unix time.
	 */
	static function FromUnixTimeSeconds(seconds:haxe.Int64):cs.system.DateTimeOffset;
	/**
	 * Adds a specified time interval to a  object that has a specified date and time,
	 * and yields a  object that has new a date and time.
	 * @param dateTimeOffset The object to add the time interval to.
	 * @param timeSpan The time interval to add.
	 * @return An object whose value is the sum of the values of  and .
	 */
	static function op_Addition(dateTimeOffset:cs.system.DateTimeOffset, timeSpan:cs.system.TimeSpan):cs.system.DateTimeOffset;
	/**
	 * Determines whether two specified  objects represent the same point in time.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if both  objects have the same  value; otherwise, .
	 */
	static function op_Equality(left:cs.system.DateTimeOffset, right:cs.system.DateTimeOffset):Bool;
	/**
	 * Determines whether one specified  object is greater than (or later than) a
	 * second specified  object.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if the  value of  is later than the  value of ; otherwise, .
	 */
	static function op_GreaterThan(left:cs.system.DateTimeOffset, right:cs.system.DateTimeOffset):Bool;
	/**
	 * Determines whether one specified  object is greater than or equal to a second
	 * specified  object.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if the  value of  is the same as or later than the  value of ;
	 * otherwise, .
	 */
	static function op_GreaterThanOrEqual(left:cs.system.DateTimeOffset, right:cs.system.DateTimeOffset):Bool;
	/**
	 * Defines an implicit conversion of a  object to a  object.
	 * @param dateTime The object to convert.
	 * @return The converted object.
	 */
	static function op_Implicit(dateTime:cs.system.DateTime):cs.system.DateTimeOffset;
	/**
	 * Determines whether two specified  objects refer to different points in time.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  and  do not have the same  value; otherwise, .
	 */
	static function op_Inequality(left:cs.system.DateTimeOffset, right:cs.system.DateTimeOffset):Bool;
	/**
	 * Determines whether one specified  object is less than a second specified 
	 * object.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if the  value of  is earlier than the  value of ; otherwise, .
	 */
	static function op_LessThan(left:cs.system.DateTimeOffset, right:cs.system.DateTimeOffset):Bool;
	/**
	 * Determines whether one specified  object is less than a second specified 
	 * object.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if the  value of  is earlier than the  value of ; otherwise, .
	 */
	static function op_LessThanOrEqual(left:cs.system.DateTimeOffset, right:cs.system.DateTimeOffset):Bool;
	@:overload(function(left:cs.system.DateTimeOffset, right:cs.system.DateTimeOffset):cs.system.TimeSpan {})
	/**
	 * Subtracts one  object from another and yields a time interval.
	 * @param left The minuend.
	 * @param right The subtrahend.
	 * @return An object that represents the difference between  and .
	 */
	static function op_Subtraction(dateTimeOffset:cs.system.DateTimeOffset, timeSpan:cs.system.TimeSpan):cs.system.DateTimeOffset;
	@:overload(function(input:String):cs.system.DateTimeOffset {})
	@:overload(function(input:String, formatProvider:cs.system.IFormatProvider):cs.system.DateTimeOffset {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, ?formatProvider:cs.system.IFormatProvider, ?styles:cs.system.globalization.DateTimeStyles):cs.system.DateTimeOffset {})
	/**
	 * @param input 
	 * @param formatProvider 
	 * @param styles 
	 */
	static function Parse(input:String, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles):cs.system.DateTimeOffset;
	@:overload(function(input:String, format:String, formatProvider:cs.system.IFormatProvider):cs.system.DateTimeOffset {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, format:cs.system.ReadOnlySpan<cs.Char16>, formatProvider:cs.system.IFormatProvider, ?styles:cs.system.globalization.DateTimeStyles):cs.system.DateTimeOffset {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, ?styles:cs.system.globalization.DateTimeStyles):cs.system.DateTimeOffset {})
	@:overload(function(input:String, format:String, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles):cs.system.DateTimeOffset {})
	/**
	 * Converts a character span that represents a date and time to its  equivalent
	 * using the specified format, culture-specific format information, and style. The
	 * format of the date and time representation must match the specified format
	 * exactly.
	 * @param input A character span that represents a date and time.
	 * @param format A character span that contains a format specifier that defines the
	 * expected format of .
	 * @param formatProvider An object that provides culture-specific formatting
	 * information about .
	 * @param styles A bitwise combination of enumeration values that indicates the
	 * permitted format of .
	 * @return An object that is equivalent to the date and time that is contained in
	 * the  parameter, as specified by the , , and  parameters.
	 */
	static function ParseExact(input:String, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles):cs.system.DateTimeOffset;
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.system.DateTimeOffset>):Bool {})
	@:overload(function(input:String, result:cs.Ref<cs.system.DateTimeOffset>):Bool {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTimeOffset>):Bool {})
	/**
	 * @param input 
	 * @param result 
	 */
	static function TryParse(input:String, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTimeOffset>):Bool;
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, format:cs.system.ReadOnlySpan<cs.Char16>, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTimeOffset>):Bool {})
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTimeOffset>):Bool {})
	@:overload(function(input:String, format:String, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTimeOffset>):Bool {})
	/**
	 * Converts the representation of a date and time in a character span to its 
	 * equivalent using the specified format, culture-specific format information, and
	 * style. The format of the date and time representation must match the specified
	 * format exactly.
	 * @param input A character span that contains the representation of a date and
	 * time to convert.
	 * @param format A format specifier that defines the required format of .
	 * @param formatProvider An object that supplies culture-specific formatting
	 * information about .
	 * @param styles A bitwise combination of enumeration values that indicates the
	 * permitted format of . A typical value to specify is
	 * @param result When the method returns, contains the  equivalent to the date and
	 * time of , if the conversion succeeded, or  if the conversion failed. The
	 * conversion fails if the
	 * @return if the  parameter is successfully converted; otherwise, .
	 */
	static function TryParseExact(input:String, formats:cs.NativeArray<String>, formatProvider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTimeOffset>):Bool;
	/**
	 * Returns a new  object that adds a specified time interval to the value of this
	 * instance.
	 * @param timeSpan A  object that represents a positive or a negative time
	 * interval.
	 * @return An object whose value is the sum of the date and time represented by the
	 * current  object and the time interval represented by .
	 */
	function Add(timeSpan:cs.system.TimeSpan):cs.system.DateTimeOffset;
	/**
	 * Returns a new  object that adds a specified number of whole and fractional days
	 * to the value of this instance.
	 * @param days A number of whole and fractional days. The number can be negative or
	 * positive.
	 * @return An object whose value is the sum of the date and time represented by the
	 * current  object and the number of days represented by .
	 */
	function AddDays(days:Float):cs.system.DateTimeOffset;
	/**
	 * Returns a new  object that adds a specified number of whole and fractional hours
	 * to the value of this instance.
	 * @param hours A number of whole and fractional hours. The number can be negative
	 * or positive.
	 * @return An object whose value is the sum of the date and time represented by the
	 * current  object and the number of hours represented by .
	 */
	function AddHours(hours:Float):cs.system.DateTimeOffset;
	/**
	 * Returns a new  object that adds a specified number of milliseconds to the value
	 * of this instance.
	 * @param milliseconds A number of whole and fractional milliseconds. The number
	 * can be negative or positive.
	 * @return An object whose value is the sum of the date and time represented by the
	 * current  object and the number of whole milliseconds represented by .
	 */
	function AddMilliseconds(milliseconds:Float):cs.system.DateTimeOffset;
	/**
	 * Returns a new  object that adds a specified number of whole and fractional
	 * minutes to the value of this instance.
	 * @param minutes A number of whole and fractional minutes. The number can be
	 * negative or positive.
	 * @return An object whose value is the sum of the date and time represented by the
	 * current  object and the number of minutes represented by .
	 */
	function AddMinutes(minutes:Float):cs.system.DateTimeOffset;
	/**
	 * Returns a new  object that adds a specified number of months to the value of
	 * this instance.
	 * @param months A number of whole months. The number can be negative or positive.
	 * @return An object whose value is the sum of the date and time represented by the
	 * current  object and the number of months represented by .
	 */
	function AddMonths(months:Int):cs.system.DateTimeOffset;
	/**
	 * Returns a new  object that adds a specified number of whole and fractional
	 * seconds to the value of this instance.
	 * @param seconds A number of whole and fractional seconds. The number can be
	 * negative or positive.
	 * @return An object whose value is the sum of the date and time represented by the
	 * current  object and the number of seconds represented by .
	 */
	function AddSeconds(seconds:Float):cs.system.DateTimeOffset;
	/**
	 * Returns a new  object that adds a specified number of ticks to the value of this
	 * instance.
	 * @param ticks A number of 100-nanosecond ticks. The number can be negative or
	 * positive.
	 * @return An object whose value is the sum of the date and time represented by the
	 * current  object and the number of ticks represented by .
	 */
	function AddTicks(ticks:haxe.Int64):cs.system.DateTimeOffset;
	/**
	 * Returns a new  object that adds a specified number of years to the value of this
	 * instance.
	 * @param years A number of years. The number can be negative or positive.
	 * @return An object whose value is the sum of the date and time represented by the
	 * current  object and the number of years represented by .
	 */
	function AddYears(years:Int):cs.system.DateTimeOffset;
	/**
	 * Compares the current  object to a specified  object and indicates whether the
	 * current object is earlier than, the same as, or later than the second  object.
	 * @param other An object to compare with the current  object.
	 * @return A signed integer that indicates the relationship between the current 
	 * object and , as the following table shows. Return Value Description Less than
	 * zero The current  object is earlier than . Zero The current  object is the same
	 * as . Greater than zero. The current  object is later than .
	 */
	function CompareTo(other:cs.system.DateTimeOffset):Int;
	@:overload(function(other:cs.system.DateTimeOffset):Bool {})
	/**
	 * Determines whether the current  object represents the same point in time as a
	 * specified  object.
	 * @param other An object to compare to the current  object.
	 * @return if both  objects have the same  value; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Determines whether the current  object represents the same time and has the same
	 * offset as a specified  object.
	 * @param other The object to compare to the current  object.
	 * @return if the current  object and  have the same date and time value and the
	 * same  value; otherwise, .
	 */
	function EqualsExact(other:cs.system.DateTimeOffset):Bool;
	/**
	 * Returns the hash code for the current  object.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	@:overload(function(value:cs.system.DateTimeOffset):cs.system.TimeSpan {})
	/**
	 * Subtracts a  value that represents a specific date and time from the current 
	 * object.
	 * @param value An object that represents the value to subtract.
	 * @return An object that specifies the interval between the two  objects.
	 */
	function Subtract(value:cs.system.TimeSpan):cs.system.DateTimeOffset;
	/**
	 * Converts the value of the current  object to a Windows file time.
	 * @return The value of the current  object, expressed as a Windows file time.
	 */
	function ToFileTime():haxe.Int64;
	/**
	 * Converts the current  object to a  object that represents the local time.
	 * @return An object that represents the date and time of the current  object
	 * converted to local time.
	 */
	function ToLocalTime():cs.system.DateTimeOffset;
	/**
	 * Converts the value of the current  object to the date and time specified by an
	 * offset value.
	 * @param offset The offset to convert the  value to.
	 * @return An object that is equal to the original  object (that is, their  methods
	 * return identical points in time) but whose  property is set to .
	 */
	function ToOffset(offset:cs.system.TimeSpan):cs.system.DateTimeOffset;
	@:overload(function():String {})
	@:overload(function(formatProvider:cs.system.IFormatProvider):String {})
	@:overload(function(format:String):String {})
	/**
	 * Converts the value of the current  object to its equivalent string
	 * representation.
	 * @return A string representation of a  object that includes the offset appended
	 * at the end of the string.
	 */
	function ToString(format:String, formatProvider:cs.system.IFormatProvider):String;
	/**
	 * Converts the current  object to a  value that represents the Coordinated
	 * Universal Time (UTC).
	 * @return An object that represents the date and time of the current  object
	 * converted to Coordinated Universal Time (UTC).
	 */
	function ToUniversalTime():cs.system.DateTimeOffset;
	/**
	 * Returns the number of milliseconds that have elapsed since
	 * 1970-01-01T00:00:00.000Z.
	 * @return The number of milliseconds that have elapsed since
	 * 1970-01-01T00:00:00.000Z.
	 */
	function ToUnixTimeMilliseconds():haxe.Int64;
	/**
	 * Returns the number of seconds that have elapsed since 1970-01-01T00:00:00Z.
	 * @return The number of seconds that have elapsed since 1970-01-01T00:00:00Z.
	 */
	function ToUnixTimeSeconds():haxe.Int64;
	/**
	 * @param destination 
	 * @param charsWritten 
	 * @param format 
	 * @param formatProvider 
	 */
	function TryFormat(destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>, ?format:cs.system.ReadOnlySpan<cs.Char16>, ?formatProvider:cs.system.IFormatProvider):Bool;
}
