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
	@see https://www.php.net/manual/en/class.numberformatter.php
**/
@:native("NumberFormatter")
extern class NumberFormatter {
	@:phpClassConst static final CURRENCY: Int;
	@:phpClassConst static final CURRENCY_ACCOUNTING: Int;
	@:phpClassConst static final CURRENCY_CODE: Int;
	@:phpClassConst static final CURRENCY_SYMBOL: Int;
	@:phpClassConst static final DECIMAL: Int;
	@:phpClassConst static final DECIMAL_ALWAYS_SHOWN: Int;
	@:phpClassConst static final DECIMAL_SEPARATOR_SYMBOL: Int;
	@:phpClassConst static final DEFAULT_RULESET: Int;
	@:phpClassConst static final DEFAULT_STYLE: Int;
	@:phpClassConst static final DIGIT_SYMBOL: Int;
	@:phpClassConst static final DURATION: Int;
	@:phpClassConst static final EXPONENTIAL_SYMBOL: Int;
	@:phpClassConst static final FORMAT_WIDTH: Int;
	@:phpClassConst static final FRACTION_DIGITS: Int;
	@:phpClassConst static final GROUPING_SEPARATOR_SYMBOL: Int;
	@:phpClassConst static final GROUPING_SIZE: Int;
	@:phpClassConst static final GROUPING_USED: Int;
	@:phpClassConst static final IGNORE: Int;
	@:phpClassConst static final INFINITY_SYMBOL: Int;
	@:phpClassConst static final INTEGER_DIGITS: Int;
	@:phpClassConst static final INTL_CURRENCY_SYMBOL: Int;
	@:phpClassConst static final LENIENT_PARSE: Int;
	@:phpClassConst static final MAX_FRACTION_DIGITS: Int;
	@:phpClassConst static final MAX_INTEGER_DIGITS: Int;
	@:phpClassConst static final MAX_SIGNIFICANT_DIGITS: Int;
	@:phpClassConst static final MIN_FRACTION_DIGITS: Int;
	@:phpClassConst static final MIN_INTEGER_DIGITS: Int;
	@:phpClassConst static final MIN_SIGNIFICANT_DIGITS: Int;
	@:phpClassConst static final MINUS_SIGN_SYMBOL: Int;
	@:phpClassConst static final MONETARY_GROUPING_SEPARATOR_SYMBOL: Int;
	@:phpClassConst static final MONETARY_SEPARATOR_SYMBOL: Int;
	@:phpClassConst static final MULTIPLIER: Int;
	@:phpClassConst static final NAN_SYMBOL: Int;
	@:phpClassConst static final NEGATIVE_PREFIX: Int;
	@:phpClassConst static final NEGATIVE_SUFFIX: Int;
	@:phpClassConst static final ORDINAL: Int;
	@:phpClassConst static final PAD_AFTER_PREFIX: Int;
	@:phpClassConst static final PAD_AFTER_SUFFIX: Int;
	@:phpClassConst static final PAD_BEFORE_PREFIX: Int;
	@:phpClassConst static final PAD_BEFORE_SUFFIX: Int;
	@:phpClassConst static final PAD_ESCAPE_SYMBOL: Int;
	@:phpClassConst static final PADDING_CHARACTER: Int;
	@:phpClassConst static final PADDING_POSITION: Int;
	@:phpClassConst static final PARSE_INT_ONLY: Int;
	@:phpClassConst static final PATTERN_DECIMAL: Int;
	@:phpClassConst static final PATTERN_RULEBASED: Int;
	@:phpClassConst static final PATTERN_SEPARATOR_SYMBOL: Int;
	@:phpClassConst static final PERCENT: Int;
	@:phpClassConst static final PERCENT_SYMBOL: Int;
	@:phpClassConst static final PERMILL_SYMBOL: Int;
	@:phpClassConst static final PLUS_SIGN_SYMBOL: Int;
	@:phpClassConst static final POSITIVE_PREFIX: Int;
	@:phpClassConst static final POSITIVE_SUFFIX: Int;
	@:phpClassConst static final PUBLIC_RULESETS: Int;
	@:phpClassConst static final ROUND_CEILING: Int;
	@:phpClassConst static final ROUND_DOWN: Int;
	@:phpClassConst static final ROUND_FLOOR: Int;
	@:phpClassConst static final ROUND_HALFDOWN: Int;
	@:phpClassConst static final ROUND_HALFEVEN: Int;
	@:phpClassConst static final ROUND_HALFUP: Int;
	@:phpClassConst static final ROUND_UP: Int;
	@:phpClassConst static final ROUNDING_INCREMENT: Int;
	@:phpClassConst static final ROUNDING_MODE: Int;
	@:phpClassConst static final SCIENTIFIC: Int;
	@:phpClassConst static final SECONDARY_GROUPING_SIZE: Int;
	@:phpClassConst static final SIGNIFICANT_DIGIT_SYMBOL: Int;
	@:phpClassConst static final SIGNIFICANT_DIGITS_USED: Int;
	@:phpClassConst static final SPELLOUT: Int;
	@:phpClassConst static final TYPE_CURRENCY: Int;
	@:phpClassConst static final TYPE_DEFAULT: Int;
	@:phpClassConst static final TYPE_DOUBLE: Int;
	@:phpClassConst static final TYPE_INT32: Int;
	@:phpClassConst static final TYPE_INT64: Int;
	@:phpClassConst static final ZERO_DIGIT_SYMBOL: Int;

	function new(locale: String, style: Int, ?pattern: String);

	static function create(locale: String, style: Int, ?pattern: String): EitherType<NumberFormatter, Bool>;

	function formatCurrency(value: Float, currency: String): EitherType<String, Bool>;
	function format(value: EitherType<Int, Float>, ?type: Int): EitherType<String, Bool>;
	function getAttribute(attr: Int): EitherType<Int, Bool>;
	function getErrorCode(): Int;
	function getErrorMessage(): String;
	function getLocale(?type: Int): String;
	function getPattern(): EitherType<String, Bool>;
	function getSymbol(attr: Int): EitherType<String, Bool>;
	function getTextAttribute(attr: Int): EitherType<String, Bool>;
	function parseCurrency(value: String, currency: Ref<String>, ?position: Ref<Int>): EitherType<Float, Bool>;
	function parse(value: String, ?type: Int, ?position: Ref<Int>): EitherType<Int, EitherType<Float, EitherType<String, Bool>>>;
	function setAttribute(attr: Int, value: Int): Bool;
	function setPattern(pattern: String): Bool;
	function setSymbol(attr: Int, value: String): Bool;
	function setTextAttribute(attr: Int, value: String): Bool;
}
