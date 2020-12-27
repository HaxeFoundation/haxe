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
	@see https://www.php.net/manual/en/class.datetimezone.php
**/
@:native("DateTimeZone")
extern class DateTimeZone {
	@:phpClassConst static final AFRICA: Int;
	@:phpClassConst static final AMERICA: Int;
	@:phpClassConst static final ANTARCTICA: Int;
	@:phpClassConst static final ARCTIC: Int;
	@:phpClassConst static final ASIA: Int;
	@:phpClassConst static final ATLANTIC: Int;
	@:phpClassConst static final AUSTRALIA: Int;
	@:phpClassConst static final EUROPE: Int;
	@:phpClassConst static final INDIAN: Int;
	@:phpClassConst static final PACIFIC: Int;
	@:phpClassConst static final UTC: Int;
	@:phpClassConst static final ALL: Int;
	@:phpClassConst static final ALL_WITH_BC: Int;
	@:phpClassConst static final PER_COUNTRY: Int;

	function new(timezone: String);
	function getLocation(): EitherType<NativeAssocArray<Dynamic>, Bool>;
	function getName(): String;
	function getOffset(datetime: DateTimeInterface): EitherType<Int, Bool>;
	function getTransitions(?timestampBegin: Int, ?timestampEnd: Int): EitherType<NativeIndexedArray<NativeAssocArray<Dynamic>>, Bool>;
	static function listAbbreviations(): EitherType<NativeAssocArray<NativeIndexedArray<NativeAssocArray<Dynamic>>>, Bool>;
	static function listIdentifiers(?timezoneGroup: Int, ?countryCode: String): EitherType<NativeIndexedArray<String>, Bool>;
}
