/*
 * Copyright (C)2005-2020 Haxe Foundation
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
	@see https://www.php.net/manual/en/class.datetimeimmutable.php
**/
@:native("DateTimeImmutable")
extern class DateTimeImmutable implements DateTimeInterface {
	@:phpClassConst static final ATOM: Int;
	@:phpClassConst static final COOKIE: Int;
	@:phpClassConst static final ISO8601: Int;
	@:phpClassConst static final RFC822: Int;
	@:phpClassConst static final RFC850: Int;
	@:phpClassConst static final RFC1036: Int;
	@:phpClassConst static final RFC1123: Int;
	@:phpClassConst static final RFC7231: Int;
	@:phpClassConst static final RFC2822: Int;
	@:phpClassConst static final RFC3339: Int;
	@:phpClassConst static final RFC3339_EXTENDED: Int;
	@:phpClassConst static final RSS: Int;
	@:phpClassConst static final W3C: Int;

	function new(datetime: String = "now", ?timezone: DateTimeZone);
	function add(interval: DateInterval): EitherType<DateTimeImmutable, Bool>;
	static function createFromFormat(format : String, datetime: String, ?timezone: DateTimeZone): EitherType<DateTimeImmutable, Bool>;
	static function createFromMutable(object: DateTime): DateTimeImmutable;
	static function getLastErrors(): NativeAssocArray<Dynamic>;
	function diff(targetObject: DateTimeInterface, absolute: Bool = false): EitherType<DateInterval, Bool>;
	function format(format: String): EitherType<String, Bool>;
	function getOffset(): EitherType<Int, Bool>;
	function getTimestamp(): Int;
	function getTimezone(): EitherType<DateTimeZone, Bool>;
	function modify(modifier: String): EitherType<DateTimeImmutable, Bool>;
	function setDate(year: Int, month: Int, day: Int): EitherType<DateTimeImmutable, Bool>;
	function setISODate(year: Int, week: Int, dayOfWeek: Int = 1): EitherType<DateTimeImmutable, Bool>;
	function setTime(hour: Int, minute: Int, second: Int = 0, microsecond: Int = 0): EitherType<DateTimeImmutable, Bool>;
	function setTimestamp(timestamp: Int): EitherType<DateTimeImmutable, Bool>;
	function setTimezone(timezone: DateTimeZone): EitherType<DateTimeImmutable, Bool>;
	function sub(interval: DateInterval): EitherType<DateTimeImmutable, Bool>;
	@:phpMagic static function __set_state(array: NativeAssocArray<Any>): DateTimeImmutable;
	@:phpMagic function __wakeup(): Void;
}
