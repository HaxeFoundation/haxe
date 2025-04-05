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

/**
	@see https://www.php.net/manual/en/class.dateperiod.php
**/
@:native("DatePeriod")
extern class DatePeriod {
	@:phpClassConst static final EXCLUDE_START_DATE: Int;

	var recurrences: Int;
	var include_start_date: Bool;
	var start: DateTimeInterface;
	var current: DateTimeInterface;
	var end: Null<DateTimeInterface>;
	var interval: DateInterval;

	@:overload(function(start: DateTimeInterface, interval: DateInterval, recurrences: Int, ?options: Int): Void {})
	@:overload(function(start: DateTimeInterface, interval: DateInterval, end: DateTimeInterface, ?options: Int): Void {})
	function new(isostr: String, ?options: Int);

	function getDateInterval(): DateInterval;
	function getEndDate(): Null<DateTimeInterface>;
	function getRecurrences(): Int;
	function getStartDate(): DateTimeInterface;
}
