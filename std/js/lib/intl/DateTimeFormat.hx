/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package js.lib.intl;

/**
	The `DateTimeFormat` object is a constructor for objects that enable language-sensitive
	date and time formatting.

	Documentation [DateTimeFormat](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Intl.DateTimeFormat")
extern class DateTimeFormat {
	@:overload(function(?locales:Array<String>, ?options:DateTimeFormatOptions):Void {})
	@:pure function new(?locales:String, ?options:DateTimeFormatOptions);

	/**
		Getter function that formats a date according to the locale and formatting options
		of this `DateTimeFormat` object.
	**/
	@:overload(function(date:js.lib.Date):String {})
	@:pure function format(date:Date):String;

	/**
		Returns an `Array` of objects representing the date string in parts that can be used
		for custom locale-aware formatting.
	**/
	@:overload(function(date:js.lib.Date):Array<DateTimeFormatPart> {})
	@:pure function formatToParts(date:Date):Array<DateTimeFormatPart>;

	/**
		Returns a new object with properties reflecting the locale and formatting options
		computed during initialization of the object.
	**/
	@:pure function resolvedOptions():DateTimeFormatResolvedOptions;

	/**
		Returns an array containing those of the provided locales that are supported
		without having to fall back to the runtime's default locale.
	**/
	@:overload(function(locales:Array<String>, ?options:DateTimeFormatSupportedLocalesOfOptions):Array<String> {})
	@:pure static function supportedLocalesOf(locales:String, ?options:DateTimeFormatSupportedLocalesOfOptions):Array<String>;
}

typedef DateTimeFormatOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	**/
	var ?localeMatcher:LocaleMatcher;

	/**
		The time zone to use. The only value implementations must recognize is `"UTC"`;
		the default is the runtime's default time zone. Implementations may also recognize
		the time zone names of the [IANA time zone database](https://www.iana.org/time-zones),
		such as `"Asia/Shanghai"`, `"Asia/Kolkata"`, `"America/New_York"`.
	**/
	var ?timeZone:String;

	/**
		Whether to use 12-hour time (as opposed to 24-hour time).
		The default is locale dependent.
		This option overrides the hc language tag and/or the `hourCycle` option in case both are present.
	**/
	var ?hour12:Bool;

	/**
		The hour cycle to use. This option overrides the `hc` language tag, if both are present,
		and the `Hour12` option takes precedence in case both options have been specified.
	**/
	var ?hourCycle:HourCycle;

	/**
		The format matching algorithm to use.
		The default is `BestFit`.
		See the following paragraphs for information about the use of this property.
	**/
	var ?formatMatcher:FormatMatcher;

	/**
		The representation of the weekday.
	**/
	var ?weekday:WeekdayRepresentation;

	/**
		The representation of the era.
	**/
	var ?era:EraRepresentation;

	/**
		The representation of the year.
	**/
	var ?year:YearRepresentation;

	/**
		The representation of the month.
	**/
	var ?month:MonthRepresentation;

	/**
		The representation of the day.
	**/
	var ?day:DayRepresentation;

	/**
		The representation of the hour.
	**/
	var ?hour:HourRepresentation;

	/**
		The representation of the minute.
	**/
	var ?minute:MinuteRepresentation;

	/**
		The representation of the second.
	**/
	var ?second:SecondRepresentation;

	/**
		The representation of the time zone name.
	**/
	var ?timeZoneName:TimeZoneName;
}

typedef DateTimeFormatResolvedOptions = {
	/**
		The BCP 47 language tag for the locale actually used.
		If any Unicode extension values were requested in the input BCP 47 language tag that led to this locale,
		the key-value pairs that were requested and are supported for this locale are included in `locale`.
	**/
	final locale:String;

	/**
		E.g. "gregory"
	**/
	final calendar:String;

	/**
		The values requested using the Unicode extension keys "ca" and "nu" or filled in as default values.
	**/
	final numberingSystem:String;

	/**
		The value provided for this property in the options argument; `undefined` (representing the runtime's
		default time zone) if none was provided.
		Warning: Applications should not rely on `undefined` being returned, as future versions may return
		a String value identifying the runtime’s default time zone instead.
	**/
	final timeZone:Null<String>;

	/**
		The value provided for this property in the `options` argument or filled in as a default.
	**/
	final hour12:Bool;

	final weekday:WeekdayRepresentation;
	final era:EraRepresentation;
	final year:YearRepresentation;
	final month:MonthRepresentation;
	final day:DayRepresentation;
	final hour:HourRepresentation;
	final minute:MinuteRepresentation;
	final second:SecondRepresentation;

	/**
		The values resulting from format matching between the corresponding properties in the `options` argument
		and the available combinations and representations for date-time formatting in the selected locale.
		Some of these properties may not be present, indicating that the corresponding components will not be
		represented in formatted output.
	**/
	final timeZoneName:TimeZoneName;
}

enum abstract HourCycle(String) {
	var H11 = "h11";
	var H12 = "h12";
	var H23 = "h23";
	var H24 = "h24";
}

enum abstract FormatMatcher(String) {
	var Basic = "basic";
	var BestFit = "best fit";
}

enum abstract WeekdayRepresentation(String) {
	/**
		(e.g., Thursday)
	 */
	var Long = "long";

	/**
		(e.g., Thu)
	 */
	var Short = "short";

	/**
		(e.g., T). Two weekdays may have the same narrow style for some locales (e.g. Tuesday's narrow style is also T).
	 */
	var Narrow = "narrow";
}

enum abstract EraRepresentation(String) {
	/**
		(e.g., Anno Domini)
	 */
	var Long = "long";

	/**
		(e.g., AD)
	 */
	var Short = "short";

	/**
		(e.g., A)
	 */
	var Narrow = "narrow";
}

enum abstract YearRepresentation(String) {
	/**
		(e.g., 2012)
	**/
	var Numeric = "numeric";

	/**
		(e.g., 12)
	**/
	var TwoDigit = "2-digit";
}

enum abstract MonthRepresentation(String) {
	/**
		(e.g., 2)
	**/
	var Numeric = "numeric";

	/**
		(e.g., 02)
	**/
	var TwoDigit = "2-digit";

	/**
		(e.g., March)
	**/
	var Long = "long";

	/**
		(e.g., Mar)
	**/
	var Short = "short";

	/**
		(e.g., M). Two months may have the same narrow style for some locales (e.g. May's narrow style is also M).
	**/
	var Narrow = "narrow";
}

enum abstract DayRepresentation(String) {
	/**
		(e.g., 1)
	**/
	var Numeric = "numeric";

	/**
		(e.g., 01)
	**/
	var TwoDigit = "2-digit";
}

enum abstract HourRepresentation(String) {
	var Numeric = "numeric";
	var TwoDigit = "2-digit";
}

enum abstract MinuteRepresentation(String) {
	var Numeric = "numeric";
	var TwoDigit = "2-digit";
}

enum abstract SecondRepresentation(String) {
	var Numeric = "numeric";
	var TwoDigit = "2-digit";
}

enum abstract TimeZoneName(String) {
	/**
		(e.g., British Summer Time)
	**/
	var Long = "long";

	/**
		(e.g., GMT+1)
	**/
	var Short = "short";
}

typedef DateTimeFormatPart = {
	var type(default, never):DateTimeFormatPartType;
	var value(default, never):String;
}

enum abstract DateTimeFormatPartType(String) {
	/**
		The string used for the day, for example "17".
	**/
	var Day = "day";

	/**
		The string used for the day period, for example, "AM" or "PM".
	**/
	var DayPeriod = "dayPeriod";

	/**
		The string used for the era, for example "BC" or "AD".
	**/
	var Era = "era";

	/**
		The string used for the hour, for example "3" or "03".
	**/
	var Hour = "hour";

	/**
		The string used for separating date and time values, for example "/", ",", "o'clock", "de", etc.
	**/
	var Literal = "literal";

	/**
		The string used for the minute, for example "00".
	**/
	var Minute = "minute";

	/**
		The string used for the month, for example "12".
	**/
	var Month = "month";

	/**
		The string used for the second, for example "07" or "42".
	**/
	var Second = "second";

	/**
		The string used for the name of the time zone, for example "UTC".
	**/
	var TimeZoneName = "timeZoneName";

	/**
		The string used for the weekday, for example "M", "Monday", or "Montag".
	**/
	var Weekday = "weekday";

	/**
		The string used for the year, for example "2012" or "96".
	**/
	var Year = "year";
}

typedef DateTimeFormatSupportedLocalesOfOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	 */
	var ?localeMatcher:LocaleMatcher;
}
