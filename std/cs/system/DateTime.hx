package cs.system;

/** Represents an instant in time, typically expressed as a date and time of day. */
@:native("System.DateTime")
extern class DateTime extends cs.system.ValueType {
	/** Represents the largest possible value of . This field is read-only. */
	static var MaxValue(default, never):cs.system.DateTime;
	/** Represents the smallest possible value of . This field is read-only. */
	static var MinValue(default, never):cs.system.DateTime;
	static var UnixEpoch(default, never):cs.system.DateTime;
	/**
	 * Gets a  object that is set to the current date and time on this computer,
	 * expressed as the local time.
	 * @return An object whose value is the current local date and time.
	 */
	static var Now(default, never):cs.system.DateTime;
	/**
	 * Gets the current date.
	 * @return An object that is set to today's date, with the time component set to
	 * 00:00:00.
	 */
	static var Today(default, never):cs.system.DateTime;
	/**
	 * Gets a  object that is set to the current date and time on this computer,
	 * expressed as the Coordinated Universal Time (UTC).
	 * @return An object whose value is the current UTC date and time.
	 */
	static var UtcNow(default, never):cs.system.DateTime;
	/**
	 * Gets the date component of this instance.
	 * @return A new object with the same date as this instance, and the time value set
	 * to 12:00:00 midnight (00:00:00).
	 */
	var Date(default, never):cs.system.DateTime;
	/**
	 * Gets the day of the month represented by this instance.
	 * @return The day component, expressed as a value between 1 and 31.
	 */
	var Day(default, never):Int;
	/**
	 * Gets the day of the week represented by this instance.
	 * @return An enumerated constant that indicates the day of the week of this 
	 * value.
	 */
	var DayOfWeek(default, never):cs.system.DayOfWeek;
	/**
	 * Gets the day of the year represented by this instance.
	 * @return The day of the year, expressed as a value between 1 and 366.
	 */
	var DayOfYear(default, never):Int;
	/**
	 * Gets the hour component of the date represented by this instance.
	 * @return The hour component, expressed as a value between 0 and 23.
	 */
	var Hour(default, never):Int;
	/**
	 * Gets a value that indicates whether the time represented by this instance is
	 * based on local time, Coordinated Universal Time (UTC), or neither.
	 * @return One of the enumeration values that indicates what the current time
	 * represents. The default is .
	 */
	var Kind(default, never):cs.system.DateTimeKind;
	/**
	 * Gets the milliseconds component of the date represented by this instance.
	 * @return The milliseconds component, expressed as a value between 0 and 999.
	 */
	var Millisecond(default, never):Int;
	/**
	 * Gets the minute component of the date represented by this instance.
	 * @return The minute component, expressed as a value between 0 and 59.
	 */
	var Minute(default, never):Int;
	/**
	 * Gets the month component of the date represented by this instance.
	 * @return The month component, expressed as a value between 1 and 12.
	 */
	var Month(default, never):Int;
	/**
	 * Gets the seconds component of the date represented by this instance.
	 * @return The seconds component, expressed as a value between 0 and 59.
	 */
	var Second(default, never):Int;
	/**
	 * Gets the number of ticks that represent the date and time of this instance.
	 * @return The number of ticks that represent the date and time of this instance.
	 * The value is between  and .
	 */
	var Ticks(default, never):haxe.Int64;
	/**
	 * Gets the time of day for this instance.
	 * @return A time interval that represents the fraction of the day that has elapsed
	 * since midnight.
	 */
	var TimeOfDay(default, never):cs.system.TimeSpan;
	/**
	 * Gets the year component of the date represented by this instance.
	 * @return The year, between 1 and 9999.
	 */
	var Year(default, never):Int;
	@:overload(function(ticks:haxe.Int64):Void {})
	@:overload(function(ticks:haxe.Int64, kind:cs.system.DateTimeKind):Void {})
	@:overload(function(year:Int, month:Int, day:Int):Void {})
	@:overload(function(year:Int, month:Int, day:Int, calendar:cs.system.globalization.Calendar):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, kind:cs.system.DateTimeKind):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, calendar:cs.system.globalization.Calendar):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, kind:cs.system.DateTimeKind):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, calendar:cs.system.globalization.Calendar):Void {})
	function new(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, calendar:cs.system.globalization.Calendar, kind:cs.system.DateTimeKind):Void;
	/**
	 * Compares two instances of  and returns an integer that indicates whether the
	 * first instance is earlier than, the same as, or later than the second instance.
	 * @param t1 The first object to compare.
	 * @param t2 The second object to compare.
	 * @return A signed number indicating the relative values of  and . Value Type
	 * Condition Less than zero is earlier than . Zero is the same as . Greater than
	 * zero is later than .
	 */
	static function Compare(t1:cs.system.DateTime, t2:cs.system.DateTime):Int;
	/**
	 * Returns the number of days in the specified month and year.
	 * @param year The year.
	 * @param month The month (a number ranging from 1 to 12).
	 * @return The number of days in  for the specified . For example, if  equals 2 for
	 * February, the return value is 28 or 29 depending upon whether  is a leap year.
	 */
	static function DaysInMonth(year:Int, month:Int):Int;
	/**
	 * Returns a value indicating whether the value of this instance is equal to the
	 * value of the specified  instance.
	 * @param value The object to compare to this instance.
	 * @return if the  parameter equals the value of this instance; otherwise, .
	 */
	static function Equals(t1:cs.system.DateTime, t2:cs.system.DateTime):Bool;
	/**
	 * Deserializes a 64-bit binary value and recreates an original serialized  object.
	 * @param dateData A 64-bit signed integer that encodes the  property in a 2-bit
	 * field and the  property in a 62-bit field.
	 * @return An object that is equivalent to the  object that was serialized by the 
	 * method.
	 */
	static function FromBinary(dateData:haxe.Int64):cs.system.DateTime;
	/**
	 * Converts the specified Windows file time to an equivalent local time.
	 * @param fileTime A Windows file time expressed in ticks.
	 * @return An object that represents the local time equivalent of the date and time
	 * represented by the  parameter.
	 */
	static function FromFileTime(fileTime:haxe.Int64):cs.system.DateTime;
	/**
	 * Converts the specified Windows file time to an equivalent UTC time.
	 * @param fileTime A Windows file time expressed in ticks.
	 * @return An object that represents the UTC time equivalent of the date and time
	 * represented by the  parameter.
	 */
	static function FromFileTimeUtc(fileTime:haxe.Int64):cs.system.DateTime;
	/**
	 * Returns a  equivalent to the specified OLE Automation Date.
	 * @param d An OLE Automation Date value.
	 * @return An object that represents the same date and time as .
	 */
	static function FromOADate(d:Float):cs.system.DateTime;
	/**
	 * Returns an indication whether the specified year is a leap year.
	 * @param year A 4-digit year.
	 * @return if  is a leap year; otherwise, .
	 */
	static function IsLeapYear(year:Int):Bool;
	/**
	 * Adds a specified time interval to a specified date and time, yielding a new date
	 * and time.
	 * @param d The date and time value to add.
	 * @param t The time interval to add.
	 * @return An object that is the sum of the values of  and .
	 */
	static function op_Addition(d:cs.system.DateTime, t:cs.system.TimeSpan):cs.system.DateTime;
	/**
	 * Determines whether two specified instances of  are equal.
	 * @param d1 The first object to compare.
	 * @param d2 The second object to compare.
	 * @return if  and  represent the same date and time; otherwise, .
	 */
	static function op_Equality(d1:cs.system.DateTime, d2:cs.system.DateTime):Bool;
	/**
	 * Determines whether one specified  is later than another specified .
	 * @param t1 The first object to compare.
	 * @param t2 The second object to compare.
	 * @return if  is later than ; otherwise, .
	 */
	static function op_GreaterThan(t1:cs.system.DateTime, t2:cs.system.DateTime):Bool;
	/**
	 * Determines whether one specified  represents a date and time that is the same as
	 * or later than another specified .
	 * @param t1 The first object to compare.
	 * @param t2 The second object to compare.
	 * @return if  is the same as or later than ; otherwise, .
	 */
	static function op_GreaterThanOrEqual(t1:cs.system.DateTime, t2:cs.system.DateTime):Bool;
	/**
	 * Determines whether two specified instances of  are not equal.
	 * @param d1 The first object to compare.
	 * @param d2 The second object to compare.
	 * @return if  and  do not represent the same date and time; otherwise, .
	 */
	static function op_Inequality(d1:cs.system.DateTime, d2:cs.system.DateTime):Bool;
	/**
	 * Determines whether one specified  is earlier than another specified .
	 * @param t1 The first object to compare.
	 * @param t2 The second object to compare.
	 * @return if  is earlier than ; otherwise, .
	 */
	static function op_LessThan(t1:cs.system.DateTime, t2:cs.system.DateTime):Bool;
	/**
	 * Determines whether one specified  represents a date and time that is the same as
	 * or earlier than another specified .
	 * @param t1 The first object to compare.
	 * @param t2 The second object to compare.
	 * @return if  is the same as or earlier than ; otherwise, .
	 */
	static function op_LessThanOrEqual(t1:cs.system.DateTime, t2:cs.system.DateTime):Bool;
	@:overload(function(d1:cs.system.DateTime, d2:cs.system.DateTime):cs.system.TimeSpan {})
	/**
	 * Subtracts a specified date and time from another specified date and time and
	 * returns a time interval.
	 * @param d1 The date and time value to subtract from (the minuend).
	 * @param d2 The date and time value to subtract (the subtrahend).
	 * @return The time interval between  and ; that is,  minus .
	 */
	static function op_Subtraction(d:cs.system.DateTime, t:cs.system.TimeSpan):cs.system.DateTime;
	@:overload(function(s:String):cs.system.DateTime {})
	@:overload(function(s:String, provider:cs.system.IFormatProvider):cs.system.DateTime {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, ?provider:cs.system.IFormatProvider, ?styles:cs.system.globalization.DateTimeStyles):cs.system.DateTime {})
	/**
	 * Converts a memory span that contains string representation of a date and time to
	 * its  equivalent by using culture-specific format information and a formatting
	 * style.
	 * @param s The memory span that contains the string to parse. See The string to
	 * parse for more information.
	 * @param provider An object that supplies culture-specific format information
	 * about .  See Parsing and cultural conventions
	 * @param styles A bitwise combination of the enumeration values that indicates the
	 * style elements that can be present in  for the parse operation to succeed, and
	 * that defines how to interpret the parsed date in relation to the current time
	 * zone or the current date. A typical value to specify is .
	 * @return An object that is equivalent to the date and time contained in , as
	 * specified by  and .
	 */
	static function Parse(s:String, provider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles):cs.system.DateTime;
	@:overload(function(s:String, format:String, provider:cs.system.IFormatProvider):cs.system.DateTime {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, format:cs.system.ReadOnlySpan<cs.Char16>, provider:cs.system.IFormatProvider, ?style:cs.system.globalization.DateTimeStyles):cs.system.DateTime {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, formats:cs.NativeArray<String>, provider:cs.system.IFormatProvider, ?style:cs.system.globalization.DateTimeStyles):cs.system.DateTime {})
	@:overload(function(s:String, format:String, provider:cs.system.IFormatProvider, style:cs.system.globalization.DateTimeStyles):cs.system.DateTime {})
	/**
	 * @param s 
	 * @param format 
	 * @param provider 
	 * @param style 
	 */
	static function ParseExact(s:String, formats:cs.NativeArray<String>, provider:cs.system.IFormatProvider, style:cs.system.globalization.DateTimeStyles):cs.system.DateTime;
	/**
	 * Creates a new  object that has the same number of ticks as the specified , but
	 * is designated as either local time, Coordinated Universal Time (UTC), or
	 * neither, as indicated by the specified  value.
	 * @param value A date and time.
	 * @param kind One of the enumeration values that indicates whether the new object
	 * represents local time, UTC, or neither.
	 * @return A new object that has the same number of ticks as the object represented
	 * by the  parameter and the  value specified by the  parameter.
	 */
	static function SpecifyKind(value:cs.system.DateTime, kind:cs.system.DateTimeKind):cs.system.DateTime;
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.system.DateTime>):Bool {})
	@:overload(function(s:String, result:cs.Ref<cs.system.DateTime>):Bool {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, provider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTime>):Bool {})
	/**
	 * @param s 
	 * @param result 
	 */
	static function TryParse(s:String, provider:cs.system.IFormatProvider, styles:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTime>):Bool;
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, format:cs.system.ReadOnlySpan<cs.Char16>, provider:cs.system.IFormatProvider, style:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTime>):Bool {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, formats:cs.NativeArray<String>, provider:cs.system.IFormatProvider, style:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTime>):Bool {})
	@:overload(function(s:String, format:String, provider:cs.system.IFormatProvider, style:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTime>):Bool {})
	/**
	 * @param s 
	 * @param format 
	 * @param provider 
	 * @param style 
	 * @param result 
	 */
	static function TryParseExact(s:String, formats:cs.NativeArray<String>, provider:cs.system.IFormatProvider, style:cs.system.globalization.DateTimeStyles, result:cs.Ref<cs.system.DateTime>):Bool;
	/**
	 * Returns a new  that adds the value of the specified  to the value of this
	 * instance.
	 * @param value A positive or negative time interval.
	 * @return An object whose value is the sum of the date and time represented by
	 * this instance and the time interval represented by .
	 */
	function Add(value:cs.system.TimeSpan):cs.system.DateTime;
	/**
	 * Returns a new  that adds the specified number of days to the value of this
	 * instance.
	 * @param value A number of whole and fractional days. The  parameter can be
	 * negative or positive.
	 * @return An object whose value is the sum of the date and time represented by
	 * this instance and the number of days represented by .
	 */
	function AddDays(value:Float):cs.system.DateTime;
	/**
	 * Returns a new  that adds the specified number of hours to the value of this
	 * instance.
	 * @param value A number of whole and fractional hours. The  parameter can be
	 * negative or positive.
	 * @return An object whose value is the sum of the date and time represented by
	 * this instance and the number of hours represented by .
	 */
	function AddHours(value:Float):cs.system.DateTime;
	/**
	 * Returns a new  that adds the specified number of milliseconds to the value of
	 * this instance.
	 * @param value A number of whole and fractional milliseconds. The  parameter can
	 * be negative or positive. Note that this value is rounded to the nearest integer.
	 * @return An object whose value is the sum of the date and time represented by
	 * this instance and the number of milliseconds represented by .
	 */
	function AddMilliseconds(value:Float):cs.system.DateTime;
	/**
	 * Returns a new  that adds the specified number of minutes to the value of this
	 * instance.
	 * @param value A number of whole and fractional minutes. The  parameter can be
	 * negative or positive.
	 * @return An object whose value is the sum of the date and time represented by
	 * this instance and the number of minutes represented by .
	 */
	function AddMinutes(value:Float):cs.system.DateTime;
	/**
	 * Returns a new  that adds the specified number of months to the value of this
	 * instance.
	 * @param months A number of months. The  parameter can be negative or positive.
	 * @return An object whose value is the sum of the date and time represented by
	 * this instance and .
	 */
	function AddMonths(months:Int):cs.system.DateTime;
	/**
	 * Returns a new  that adds the specified number of seconds to the value of this
	 * instance.
	 * @param value A number of whole and fractional seconds. The  parameter can be
	 * negative or positive.
	 * @return An object whose value is the sum of the date and time represented by
	 * this instance and the number of seconds represented by .
	 */
	function AddSeconds(value:Float):cs.system.DateTime;
	/**
	 * Returns a new  that adds the specified number of ticks to the value of this
	 * instance.
	 * @param value A number of 100-nanosecond ticks. The  parameter can be positive or
	 * negative.
	 * @return An object whose value is the sum of the date and time represented by
	 * this instance and the time represented by .
	 */
	function AddTicks(value:haxe.Int64):cs.system.DateTime;
	/**
	 * Returns a new  that adds the specified number of years to the value of this
	 * instance.
	 * @param value A number of years. The  parameter can be negative or positive.
	 * @return An object whose value is the sum of the date and time represented by
	 * this instance and the number of years represented by .
	 */
	function AddYears(value:Int):cs.system.DateTime;
	@:overload(function(value:cs.system.DateTime):Int {})
	/**
	 * Compares the value of this instance to a specified  value and returns an integer
	 * that indicates whether this instance is earlier than, the same as, or later than
	 * the specified  value.
	 * @param value The object to compare to the current instance.
	 * @return A signed number indicating the relative values of this instance and the 
	 * parameter. Value Description Less than zero This instance is earlier than . Zero
	 * This instance is the same as . Greater than zero This instance is later than .
	 */
	function CompareTo(value:Dynamic):Int;
	@:overload(function(value:cs.system.DateTime):Bool {})
	/**
	 * Returns a value indicating whether the value of this instance is equal to the
	 * value of the specified  instance.
	 * @param value The object to compare to this instance.
	 * @return if the  parameter equals the value of this instance; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	@:overload(function():cs.NativeArray<String> {})
	@:overload(function(format:cs.Char16):cs.NativeArray<String> {})
	@:overload(function(provider:cs.system.IFormatProvider):cs.NativeArray<String> {})
	/**
	 * Converts the value of this instance to all the string representations supported
	 * by the standard date and time format specifiers.
	 * @return A string array where each element is the representation of the value of
	 * this instance formatted with one of the standard date and time format
	 * specifiers.
	 */
	function GetDateTimeFormats(format:cs.Char16, provider:cs.system.IFormatProvider):cs.NativeArray<String>;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the  for value type .
	 * @return The enumerated constant, .
	 */
	function GetTypeCode():cs.system.TypeCode;
	/**
	 * Indicates whether this instance of  is within the daylight saving time range for
	 * the current time zone.
	 * @return if the value of the  property is  or  and the value of this instance of 
	 * is within the daylight saving time range for the local time zone;  if  is .
	 */
	function IsDaylightSavingTime():Bool;
	@:overload(function(value:cs.system.DateTime):cs.system.TimeSpan {})
	/**
	 * Returns a new  that subtracts the specified date and time from the value of this
	 * instance.
	 * @param value The date and time value to subtract.
	 * @return A time interval that is equal to the date and time represented by this
	 * instance minus the date and time represented by .
	 */
	function Subtract(value:cs.system.TimeSpan):cs.system.DateTime;
	/**
	 * Serializes the current  object to a 64-bit binary value that subsequently can be
	 * used to recreate the  object.
	 * @return A 64-bit signed integer that encodes the  and  properties.
	 */
	function ToBinary():haxe.Int64;
	/**
	 * Converts the value of the current  object to a Windows file time.
	 * @return The value of the current  object expressed as a Windows file time.
	 */
	function ToFileTime():haxe.Int64;
	/**
	 * Converts the value of the current  object to a Windows file time.
	 * @return The value of the current  object expressed as a Windows file time.
	 */
	function ToFileTimeUtc():haxe.Int64;
	/**
	 * Converts the value of the current  object to local time.
	 * @return An object whose  property is , and whose value is the local time
	 * equivalent to the value of the current  object, or  if the converted value is
	 * too large to be represented by a  object, or  if the converted value is too
	 * small to be represented as a  object.
	 */
	function ToLocalTime():cs.system.DateTime;
	/**
	 * Converts the value of the current  object to its equivalent long date string
	 * representation.
	 * @return A string that contains the long date string representation of the
	 * current  object.
	 */
	function ToLongDateString():String;
	/**
	 * Converts the value of the current  object to its equivalent long time string
	 * representation.
	 * @return A string that contains the long time string representation of the
	 * current  object.
	 */
	function ToLongTimeString():String;
	/**
	 * Converts the value of this instance to the equivalent OLE Automation date.
	 * @return A double-precision floating-point number that contains an OLE Automation
	 * date equivalent to the value of this instance.
	 */
	function ToOADate():Float;
	/**
	 * Converts the value of the current  object to its equivalent short date string
	 * representation.
	 * @return A string that contains the short date string representation of the
	 * current  object.
	 */
	function ToShortDateString():String;
	/**
	 * Converts the value of the current  object to its equivalent short time string
	 * representation.
	 * @return A string that contains the short time string representation of the
	 * current  object.
	 */
	function ToShortTimeString():String;
	@:overload(function():String {})
	@:overload(function(provider:cs.system.IFormatProvider):String {})
	@:overload(function(format:String):String {})
	/**
	 * Converts the value of the current  object to its equivalent string
	 * representation using the formatting conventions of the current culture.
	 * @return A string representation of the value of the current  object.
	 */
	function ToString(format:String, provider:cs.system.IFormatProvider):String;
	/**
	 * Converts the value of the current  object to Coordinated Universal Time (UTC).
	 * @return An object whose  property is , and whose value is the UTC equivalent to
	 * the value of the current  object, or  if the converted value is too large to be
	 * represented by a  object, or  if the converted value is too small to be
	 * represented by a  object.
	 */
	function ToUniversalTime():cs.system.DateTime;
	/**
	 * @param destination 
	 * @param charsWritten 
	 * @param format 
	 * @param provider 
	 */
	function TryFormat(destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>, ?format:cs.system.ReadOnlySpan<cs.Char16>, ?provider:cs.system.IFormatProvider):Bool;
}
