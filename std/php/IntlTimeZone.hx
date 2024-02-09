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
	@see https://www.php.net/manual/en/class.intltimezone.php
**/
@:native("IntlTimeZone")
extern class IntlTimeZone {
	@:phpClassConst static final DISPLAY_LONG: Int;
	@:phpClassConst static final DISPLAY_SHORT: Int;

	static function countEquivalentIDs(zoneId: String): Int;
	static function createDefault(): IntlTimeZone;
	static function createEnumeration(countryOrRawOffset: Any): IntlIterator<Int, String>;
	static function createTimeZone(zoneId: String): IntlTimeZone;
	static function createTimeZoneIDEnumeration(zoneType: Int, ?region: String, ?rawOffset: Int): EitherType<IntlIterator<Int, String>, Bool>;
	static function fromDateTimeZone(zoneId: DateTimeZone): IntlTimeZone;
	static function getCanonicalID(zoneId: String, isSystemID: Ref<Bool>): String;
	static function getEquivalentID(zoneId: String, index: Int): String;
	static function getGMT(): IntlTimeZone;
	static function getIDForWindowsID(timezone: String, ?region: String): EitherType<String, Bool>;
	static function getRegion(zoneId: String): EitherType<String, Bool>;
	static function getTZDataVersion(): String;
	static function getUnknown(): Null<IntlTimeZone>;
	static function getWindowsID(timezone: String): EitherType<String, Bool>;

	function getDisplayName(?isDaylight: Bool, ?style: Int, ?locale: String): String;
	function getDSTSavings(): Int;
	function getErrorCode(): Int;
	function getErrorMessage(): String;
	function getID(): String;
	function getOffset(date: Float, local: Bool, rawOffset: Ref<Int>, dstOffset: Ref<Int>): Bool;
	function getRawOffset(): Int;
	function hasSameRules(otherTimeZone: IntlTimeZone): Bool;
	function toDateTimeZone(): DateTimeZone;
	function useDaylightTime(): Bool;
}
