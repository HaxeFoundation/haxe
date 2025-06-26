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
	@see https://www.php.net/manual/en/class.intldateformatter.php
**/
@:native("IntlDateFormatter")
extern class IntlDateFormatter {
	@:phpClassConst static final FULL: Int;
	@:phpClassConst static final GREGORIAN: Int;
	@:phpClassConst static final LONG: Int;
	@:phpClassConst static final MEDIUM: Int;
	@:phpClassConst static final NONE: Int;
	@:phpClassConst static final RELATIVE_FULL: Int;
	@:phpClassConst static final RELATIVE_LONG: Int;
	@:phpClassConst static final RELATIVE_MEDIUM: Int;
	@:phpClassConst static final RELATIVE_SHORT: Int;
	@:phpClassConst static final SHORT: Int;
	@:phpClassConst static final TRADITIONAL: Int;

	function new(locale: Null<String>, dateType: Int, timeType: Int, timezone: EitherType<String, EitherType<DateTimeZone, IntlTimeZone>> = null,
		calendar: EitherType<Int, IntlCalendar> = null, pattern: String = "");

	static function create(locale: Null<String>, dateType: Int, timeType: Int,
		timezone: EitherType<String, EitherType<DateTimeZone, IntlTimeZone>> = null,
		calendar: EitherType<Int, IntlCalendar> = null, pattern: String = ""): Null<IntlDateFormatter>;

	static function formatObject(datetime: EitherType<IntlCalendar, DateTime>, format: EitherType<Int, EitherType<NativeIndexedArray<Int>, String>> = null,
		locale: String = null): EitherType<String, Bool>;

	function format(datetime: EitherType<Int, EitherType<Float, EitherType<String, EitherType<DateTimeInterface, EitherType<IntlCalendar, NativeAssocArray<Int>>>>>>): EitherType<String, Bool>;
	function getCalendar(): EitherType<Int, Bool>;
	function getCalendarObject(): EitherType<Null<IntlCalendar>, Bool>;
	function getDateType(): EitherType<Int, Bool>;
	function getErrorCode(): Int;
	function getErrorMessage(): String;
	function getLocale(?type: Int): EitherType<String, Bool>;
	function getPattern(): EitherType<String, Bool>;
	function getTimeType(): EitherType<Int, Bool>;
	function getTimeZone(): EitherType<IntlTimeZone, Bool>;
	function getTimeZoneId(): EitherType<String, Bool>;
	function isLenient(): Bool;
	function localtime(string: String, offset: Ref<Int> = null): EitherType<NativeAssocArray<Int>, Bool>;
	function parse(string: String, offset: Ref<Int> = null): EitherType<Int, EitherType<Float, Bool>>;
	function setCalendar(calendar: EitherType<Null<IntlCalendar>, Int>): Bool;
	function setLenient(lenient: Bool): Bool;
	function setPattern(pattern: String): Bool;
	function setTimeZone(timezone: EitherType<Null<String>, EitherType<IntlTimeZone, DateTimeZone>>): Null<Bool>;
}
