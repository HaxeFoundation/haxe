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
	@see https://www.php.net/manual/en/class.collator.php
**/
@:native("Collator")
extern class Collator {
	@:phpClassConst static final ALTERNATE_HANDLING: Int;
	@:phpClassConst static final CASE_FIRST: Int;
	@:phpClassConst static final CASE_LEVEL: Int;
	@:phpClassConst static final DEFAULT_STRENGTH: Int;
	@:phpClassConst static final DEFAULT_VALUE: Int;
	@:phpClassConst static final FRENCH_COLLATION: Int;
	@:phpClassConst static final HIRAGANA_QUATERNARY_MODE: Int;
	@:phpClassConst static final IDENTICAL: Int;
	@:phpClassConst static final LOWER_FIRST: Int;
	@:phpClassConst static final NON_IGNORABLE: Int;
	@:phpClassConst static final NORMALIZATION_MODE: Int;
	@:phpClassConst static final NUMERIC_COLLATION: Int;
	@:phpClassConst static final OFF: Int;
	@:phpClassConst static final ON: Int;
	@:phpClassConst static final PRIMARY: Int;
	@:phpClassConst static final QUATERNARY: Int;
	@:phpClassConst static final SECONDARY: Int;
	@:phpClassConst static final SHIFTED: Int;
	@:phpClassConst static final SORT_NUMERIC: Int;
	@:phpClassConst static final SORT_REGULAR: Int;
	@:phpClassConst static final SORT_STRING: Int;
	@:phpClassConst static final STRENGTH: Int;
	@:phpClassConst static final TERTIARY: Int;
	@:phpClassConst static final UPPER_FIRST: Int;

	function new(locale: String);
	function asort(arr: Ref<NativeAssocArray<String>>, ?sort_flag: Int): Bool;
	function compare(str1: String, str2: String): EitherType<Int, Bool>;
	static function create(locale: String): Null<Collator>;
	function getAttribute(attr: Int): EitherType<Int, Bool>;
	function getErrorCode(): Int;
	function getErrorMessage(): String;
	function getLocale(type: Int): EitherType<String, Bool>;
	function getSortKey(str: String): EitherType<String, Bool>;
	function getStrength(): EitherType<Int, Bool>;
	function setAttribute(attr: Int, val: Int): Bool;
	function setStrength(strength: Int): Bool;
	function sort(arr: Ref<NativeArray>, ?sort_flag: Int): Bool;
	function sortWithSortKeys(arr: Ref<NativeArray>): Bool;
}
