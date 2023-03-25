/*
 * Copyright (C)2005-2021 Haxe Foundation
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

package php;

import haxe.extern.EitherType;

/**
	@see https://www.php.net/manual/en/class.intlcalendar.php
**/
@:native("IntlCalendar")
extern class IntlCalendar {
	@:phpClassConst static final DOW_FRIDAY: Int;
	@:phpClassConst static final DOW_MONDAY: Int;
	@:phpClassConst static final DOW_SATURDAY: Int;
	@:phpClassConst static final DOW_SUNDAY: Int;
	@:phpClassConst static final DOW_THURSDAY: Int;
	@:phpClassConst static final DOW_TUESDAY: Int;
	@:phpClassConst static final DOW_TYPE_WEEKDAY: Int;
	@:phpClassConst static final DOW_TYPE_WEEKEND_CEASE: Int;
	@:phpClassConst static final DOW_TYPE_WEEKEND_OFFSET: Int;
	@:phpClassConst static final DOW_TYPE_WEEKEND: Int;
	@:phpClassConst static final DOW_WEDNESDAY: Int;
	@:phpClassConst static final FIELD_AM_PM: Int;
	@:phpClassConst static final FIELD_DATE: Int;
	@:phpClassConst static final FIELD_DAY_OF_MONTH: Int;
	@:phpClassConst static final FIELD_DAY_OF_WEEK_IN_MONTH: Int;
	@:phpClassConst static final FIELD_DAY_OF_WEEK: Int;
	@:phpClassConst static final FIELD_DAY_OF_YEAR: Int;
	@:phpClassConst static final FIELD_DOW_LOCAL: Int;
	@:phpClassConst static final FIELD_DST_OFFSET: Int;
	@:phpClassConst static final FIELD_ERA: Int;
	@:phpClassConst static final FIELD_EXTENDED_YEAR: Int;
	@:phpClassConst static final FIELD_FIELD_COUNT: Int;
	@:phpClassConst static final FIELD_HOUR_OF_DAY: Int;
	@:phpClassConst static final FIELD_HOUR: Int;
	@:phpClassConst static final FIELD_IS_LEAP_MONTH: Int;
	@:phpClassConst static final FIELD_JULIAN_DAY: Int;
	@:phpClassConst static final FIELD_MILLISECOND: Int;
	@:phpClassConst static final FIELD_MILLISECONDS_IN_DAY: Int;
	@:phpClassConst static final FIELD_MINUTE: Int;
	@:phpClassConst static final FIELD_MONTH: Int;
	@:phpClassConst static final FIELD_SECOND: Int;
	@:phpClassConst static final FIELD_WEEK_OF_MONTH: Int;
	@:phpClassConst static final FIELD_WEEK_OF_YEAR: Int;
	@:phpClassConst static final FIELD_YEAR_WOY: Int;
	@:phpClassConst static final FIELD_YEAR: Int;
	@:phpClassConst static final FIELD_ZONE_OFFSET: Int;
	@:phpClassConst static final WALLTIME_FIRST: Int;
	@:phpClassConst static final WALLTIME_LAST: Int;
	@:phpClassConst static final WALLTIME_NEXT_VALID: Int;

	static function createInstance(timeZone: EitherType<Null<String>, EitherType<DateTimeZone, IntlTimeZone>> = null, locale: Null<String> = ""): Null<IntlCalendar>;
	static function fromDateTime(dateTime: EitherType<DateTime, String>): Null<IntlCalendar>;
	static function getAvailableLocales(): NativeIndexedArray<String>;
	static function getKeywordValuesForLocale(key: String, locale: String, commonlyUsed: Bool): EitherType<NativeIterator<Int, String>, Bool>;
	static function getNow(): Float;

	function add(field: Int, amount: Int): Bool;
	function after(other: IntlCalendar): Bool;
	function before(other: IntlCalendar): Bool;
	function clear(field: Null<Int> = null): Bool;
	function equals(other: IntlCalendar): Bool;
	function fieldDifference(when: Float, field: Int): EitherType<Int, Bool>;
	function get(field: Int): Int;
	function getActualMaximum(field: Int): EitherType<Int, Bool>;
	function getActualMinimum(field: Int): EitherType<Int, Bool>;
	function getDayOfWeekType(dayOfWeek: Int): EitherType<Int, Bool>;
	function getErrorCode(): Int;
	function getErrorMessage(): String;
	function getFirstDayOfWeek(): EitherType<Int, Bool>;
	function getGreatestMinimum(field: Int): EitherType<Int, Bool>;
	function getLeastMaximum(field: Int): EitherType<Int, Bool>;
	function getLocale(localeType: Int): EitherType<String, Bool>;
	function getMaximum(field: Int): EitherType<Int, Bool>;
	function getMinimalDaysInFirstWeek(): EitherType<Int, Bool>;
	function getMinimum(field: Int): EitherType<Int, Bool>;
	function getRepeatedWallTimeOption(): Int;
	function getSkippedWallTimeOption(): Int;
	function getTime(): Float;
	function getTimeZone(): IntlTimeZone;
	function getType(): String;
	function getWeekendTransition(dayOfWeek: Int): EitherType<Int, Bool>;
	function inDaylightTime(): Bool;
	function isEquivalentTo(other: IntlCalendar): Bool;
	function isLenient(): Bool;
	function isSet(field: Int): Bool;
	function isWeekend(date: Null<Float> = null): Bool;
	function roll(field: Int, amountOrUpOrDown: EitherType<Bool, Int>): Bool;
	function setFirstDayOfWeek(dayOfWeek: Int): Bool;
	function setLenient(isLenient: Bool): Bool;
	function setMinimalDaysInFirstWeek(minimalDays: Int): Bool;
	function setRepeatedWallTimeOption(wallTimeOption: Int): Bool;
	function setSkippedWallTimeOption(wallTimeOption: Int): Bool;
	function setTime(date: Float): Bool;
	function setTimeZone(timeZone: EitherType<Null<String>, EitherType<DateTimeZone, IntlTimeZone>>): Bool;
	function toDateTime(): EitherType<DateTime, Bool>;

	@:overload(function(year: Int, month: Int, dayOfMonth: Null<Int> = null, hour: Null<Int> = null, minute: Null<Int> = null, second: Null<Int> = null): Bool {})
	function set(field: Int, value: Int): Bool;
}
