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
	@see https://www.php.net/manual/en/class.locale.php
**/
@:native("Locale")
extern class Locale {
	@:phpClassConst static final ACTUAL_LOCALE: String;
	@:phpClassConst static final DEFAULT_LOCALE: Null<String>;
	@:phpClassConst static final EXTLANG_TAG: String;
	@:phpClassConst static final GRANDFATHERED_LANG_TAG: String;
	@:phpClassConst static final LANG_TAG: String;
	@:phpClassConst static final PRIVATE_TAG: String;
	@:phpClassConst static final REGION_TAG: String;
	@:phpClassConst static final SCRIPT_TAG: String;
	@:phpClassConst static final VALID_LOCALE: String;
	@:phpClassConst static final VARIANT_TAG: String;

	static function acceptFromHttp(header: String): String;
	static function canonicalize(locale: String): String;
	static function composeLocale(subtags: NativeAssocArray<String>): String;
	static function filterMatches(langtag: String, locale: String, canonicalize: Bool = false): Bool;
	static function getAllVariants(locale: String): Null<NativeIndexedArray<String>>;
	static function getDefault(): String;
	static function getDisplayLanguage(locale: String, ?in_locale: String): String;
	static function getDisplayName(locale: String, ?in_locale: String): String;
	static function getDisplayRegion(locale: String, ?in_locale: String): String;
	static function getDisplayScript(locale: String, ?in_locale: String): String;
	static function getDisplayVariant(locale: String, ?in_locale: String): String;
	static function getKeywords(locale: String): NativeAssocArray<String>;
	static function getPrimaryLanguage(locale: String): Null<String>;
	static function getRegion(locale: String): Null<String>;
	static function getScript(locale: String): Null<String>;
	static function lookup(langtag: NativeIndexedArray<String>, locale: String, canonicalize: Bool = false, ?defaultLocale: String): String;
	static function parseLocale(locale: String): Null<NativeAssocArray<String>>;
	static function setDefault(locale: String): Bool;
}
