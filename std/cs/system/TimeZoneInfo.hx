package cs.system;

/** Represents any time zone in the world. */
@:native("System.TimeZoneInfo")
extern class TimeZoneInfo {
	/**
	 * Gets a  object that represents the local time zone.
	 * @return An object that represents the local time zone.
	 */
	static var Local(default, never):cs.system.TimeZoneInfo;
	/**
	 * Gets a  object that represents the Coordinated Universal Time (UTC) zone.
	 * @return An object that represents the Coordinated Universal Time (UTC) zone.
	 */
	static var Utc(default, never):cs.system.TimeZoneInfo;
	/**
	 * Gets the time difference between the current time zone's standard time and
	 * Coordinated Universal Time (UTC).
	 * @return An object that indicates the time difference between the current time
	 * zone's standard time and Coordinated Universal Time (UTC).
	 */
	var BaseUtcOffset(default, never):cs.system.TimeSpan;
	/**
	 * Gets the display name for the current time zone's daylight saving time.
	 * @return The display name for the time zone's daylight saving time.
	 */
	var DaylightName(default, never):String;
	/**
	 * Gets the general display name that represents the time zone.
	 * @return The time zone's general display name.
	 */
	var DisplayName(default, never):String;
	/**
	 * Gets the time zone identifier.
	 * @return The time zone identifier.
	 */
	var Id(default, never):String;
	/**
	 * Gets the display name for the time zone's standard time.
	 * @return The display name of the time zone's standard time.
	 */
	var StandardName(default, never):String;
	/**
	 * Gets a value indicating whether the time zone has any daylight saving time
	 * rules.
	 * @return if the time zone supports daylight saving time; otherwise, .
	 */
	var SupportsDaylightSavingTime(default, never):Bool;
	/** Clears cached time zone data. */
	static function ClearCachedData():Void;
	@:overload(function(dateTime:cs.system.DateTime, destinationTimeZone:cs.system.TimeZoneInfo):cs.system.DateTime {})
	@:overload(function(dateTimeOffset:cs.system.DateTimeOffset, destinationTimeZone:cs.system.TimeZoneInfo):cs.system.DateTimeOffset {})
	/**
	 * Converts a time to the time in a particular time zone.
	 * @param dateTime The date and time to convert.
	 * @param destinationTimeZone The time zone to convert  to.
	 * @return The date and time in the destination time zone.
	 */
	static function ConvertTime(dateTime:cs.system.DateTime, sourceTimeZone:cs.system.TimeZoneInfo, destinationTimeZone:cs.system.TimeZoneInfo):cs.system.DateTime;
	@:overload(function(dateTime:cs.system.DateTime, destinationTimeZoneId:String):cs.system.DateTime {})
	@:overload(function(dateTimeOffset:cs.system.DateTimeOffset, destinationTimeZoneId:String):cs.system.DateTimeOffset {})
	/**
	 * Converts a time to the time in another time zone based on the time zone's
	 * identifier.
	 * @param dateTime The date and time to convert.
	 * @param destinationTimeZoneId The identifier of the destination time zone.
	 * @return The date and time in the destination time zone.
	 */
	static function ConvertTimeBySystemTimeZoneId(dateTime:cs.system.DateTime, sourceTimeZoneId:String, destinationTimeZoneId:String):cs.system.DateTime;
	/**
	 * Converts a Coordinated Universal Time (UTC) to the time in a specified time
	 * zone.
	 * @param dateTime The Coordinated Universal Time (UTC).
	 * @param destinationTimeZone The time zone to convert  to.
	 * @return The date and time in the destination time zone. Its  property is  if  is
	 * ; otherwise, its  property is .
	 */
	static function ConvertTimeFromUtc(dateTime:cs.system.DateTime, destinationTimeZone:cs.system.TimeZoneInfo):cs.system.DateTime;
	@:overload(function(dateTime:cs.system.DateTime):cs.system.DateTime {})
	/**
	 * Converts the specified date and time to Coordinated Universal Time (UTC).
	 * @param dateTime The date and time to convert.
	 * @return The Coordinated Universal Time (UTC) that corresponds to the  parameter.
	 * The  value's  property is always set to .
	 */
	static function ConvertTimeToUtc(dateTime:cs.system.DateTime, sourceTimeZone:cs.system.TimeZoneInfo):cs.system.DateTime;
	@:overload(function(id:String, baseUtcOffset:cs.system.TimeSpan, displayName:String, standardDisplayName:String):cs.system.TimeZoneInfo {})
	@:overload(function(id:String, baseUtcOffset:cs.system.TimeSpan, displayName:String, standardDisplayName:String, daylightDisplayName:String, adjustmentRules:cs.NativeArray<cs.system.TimeZoneInfo_AdjustmentRule>):cs.system.TimeZoneInfo {})
	/**
	 * Creates a custom time zone with a specified identifier, an offset from
	 * Coordinated Universal Time (UTC), a display name, and a standard time display
	 * name.
	 * @param id The time zone's identifier.
	 * @param baseUtcOffset An object that represents the time difference between this
	 * time zone and Coordinated Universal Time (UTC).
	 * @param displayName The display name of the new time zone.
	 * @param standardDisplayName The name of the new time zone's standard time.
	 * @return The new time zone.
	 */
	static function CreateCustomTimeZone(id:String, baseUtcOffset:cs.system.TimeSpan, displayName:String, standardDisplayName:String, daylightDisplayName:String, adjustmentRules:cs.NativeArray<cs.system.TimeZoneInfo_AdjustmentRule>, disableDaylightSavingTime:Bool):cs.system.TimeZoneInfo;
	/**
	 * Instantiates a new  object based on its identifier.
	 * @param id The time zone identifier, which corresponds to the  property.
	 * @return An object whose identifier is the value of the  parameter.
	 */
	static function FindSystemTimeZoneById(id:String):cs.system.TimeZoneInfo;
	/**
	 * Deserializes a string to re-create an original serialized  object.
	 * @param source The string representation of the serialized  object.
	 * @return The original serialized object.
	 */
	static function FromSerializedString(source:String):cs.system.TimeZoneInfo;
	/**
	 * Returns a sorted collection of all the time zones about which information is
	 * available on the local system.
	 * @return A read-only collection of  objects.
	 */
	static function GetSystemTimeZones():cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.TimeZoneInfo>;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Determines whether the current  object and another object are equal.
	 * @param obj A second object to compare with the current object.
	 * @return if  is a  object that is equal to the current instance; otherwise, .
	 */
	function Equals(other:cs.system.TimeZoneInfo):Bool;
	/**
	 * Retrieves an array of  objects that apply to the current  object.
	 * @return An array of objects for this time zone.
	 */
	function GetAdjustmentRules():cs.NativeArray<cs.system.TimeZoneInfo_AdjustmentRule>;
	@:overload(function(dateTime:cs.system.DateTime):cs.NativeArray<cs.system.TimeSpan> {})
	/**
	 * Returns information about the possible dates and times that an ambiguous date
	 * and time can be mapped to.
	 * @param dateTime A date and time.
	 * @return An array of objects that represents possible Coordinated Universal Time
	 * (UTC) offsets that a particular date and time can be mapped to.
	 */
	function GetAmbiguousTimeOffsets(dateTimeOffset:cs.system.DateTimeOffset):cs.NativeArray<cs.system.TimeSpan>;
	/**
	 * Serves as a hash function for hashing algorithms and data structures such as
	 * hash tables.
	 * @return A 32-bit signed integer that serves as the hash code for this  object.
	 */
	function GetHashCode():Int;
	@:overload(function(dateTime:cs.system.DateTime):cs.system.TimeSpan {})
	/**
	 * Calculates the offset or difference between the time in this time zone and
	 * Coordinated Universal Time (UTC) for a particular date and time.
	 * @param dateTime The date and time to determine the offset for.
	 * @return An object that indicates the time difference between the two time zones.
	 */
	function GetUtcOffset(dateTimeOffset:cs.system.DateTimeOffset):cs.system.TimeSpan;
	/**
	 * Indicates whether the current object and another  object have the same
	 * adjustment rules.
	 * @param other A second object to compare with the current  object.
	 * @return if the two time zones have identical adjustment rules and an identical
	 * base offset; otherwise, .
	 */
	function HasSameRules(other:cs.system.TimeZoneInfo):Bool;
	@:overload(function(dateTime:cs.system.DateTime):Bool {})
	/**
	 * Determines whether a particular date and time in a particular time zone is
	 * ambiguous and can be mapped to two or more Coordinated Universal Time (UTC)
	 * times.
	 * @param dateTime A date and time value.
	 * @return if the  parameter is ambiguous; otherwise, .
	 */
	function IsAmbiguousTime(dateTimeOffset:cs.system.DateTimeOffset):Bool;
	@:overload(function(dateTime:cs.system.DateTime):Bool {})
	/**
	 * Indicates whether a specified date and time falls in the range of daylight
	 * saving time for the time zone of the current  object.
	 * @param dateTime A date and time value.
	 * @return if the  parameter is a daylight saving time; otherwise, .
	 */
	function IsDaylightSavingTime(dateTimeOffset:cs.system.DateTimeOffset):Bool;
	/**
	 * Indicates whether a particular date and time is invalid.
	 * @param dateTime A date and time value.
	 * @return if  is invalid; otherwise, .
	 */
	function IsInvalidTime(dateTime:cs.system.DateTime):Bool;
	/**
	 * Converts the current  object to a serialized string.
	 * @return A string that represents the current  object.
	 */
	function ToSerializedString():String;
	/**
	 * Returns the current  object's display name.
	 * @return The value of the  property of the current  object.
	 */
	function ToString():String;
}
