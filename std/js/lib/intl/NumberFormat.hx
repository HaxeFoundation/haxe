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
	The `NumberFormat` object is a constructor for objects that enable language sensitive number formatting.

	Documentation [NumberFormat](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NumberFormat) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NumberFormat$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Intl.NumberFormat")
extern class NumberFormat {
	@:overload(function(?locales:Array<String>, ?options:NumberFormatOptions):Void {})
	@:pure function new(?locales:String, ?options:NumberFormatOptions);

	/**
		Getter function that formats a number according to the locale
		and formatting options of this `NumberFormat` object.
	**/
	@:pure function format(number:Float):String;

	/**
		Returns an `Array` of objects representing the number string in parts
		that can be used for custom locale-aware formatting.
	**/
	@:pure function formatToParts(?number:Float):Array<NumberFormatPart>;

	/**
		Returns a new object with properties reflecting the locale and collation options
		computed during initialization of the object.
	**/
	@:pure function resolvedOptions():NumberFormatResolvedOption;

	/**
		Returns an array containing those of the provided locales that are supported
		without having to fall back to the runtime's default locale.
	**/
	@:overload(function(locales:Array<String>, ?options:NumberFormatSupportedLocalesOfOptions):Array<String> {})
	@:pure static function supportedLocalesOf(locales:String, ?options:NumberFormatSupportedLocalesOfOptions):Array<String>;
}

typedef NumberFormatOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
		For information about this option, see the [Intl page](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl#Locale_negotiation).
	**/
	var ?localeMatcher:LocaleMatcher;

	/**
		The formatting style to use.
		The default is `Decimal`.
	**/
	var ?style:NumberFormatStyle;

	/**
		The currency to use in currency formatting. Possible values are the ISO 4217 currency codes,
		such as "USD" for the US dollar, "EUR" for the euro, or "CNY" for the Chinese RMB — see the
		[Current currency & funds code list](https://www.currency-iso.org/en/home/tables/table-a1.html).
		There is no default value; if the style is "currency", the currency property must be provided.
	**/
	var ?currency:String;

	/**
		How to display the currency in currency formatting.
		The default is `Symbol`.
	**/
	var ?currencyDisplay:CurrencyDisplay;

	/**
		Whether to use grouping separators, such as thousands separators or thousand/lakh/crore separators.
		The default is `true`.
	**/
	var ?useGrouping:Bool;

	/**
		The minimum number of integer digits to use.
		Possible values are from 1 to 21; the default is 1.
	**/
	var ?minimumIntegerDigits:Int;

	/**
		The minimum number of fraction digits to use.
		Possible values are from 0 to 20; the default for plain number and percent formatting is 0;
		the default for currency formatting is the number of minor unit digits provided by the
		[ISO 4217 currency code list](http://www.currency-iso.org/en/home/tables/table-a1.html)
		(2 if the list doesn't provide that information).
	**/
	var ?minimumFractionDigits:Int;

	/**
		The maximum number of fraction digits to use.
		Possible values are from 0 to 20; the default for plain number formatting is the larger of
		minimumFractionDigits and 3; the default for currency formatting is the larger of minimumFractionDigits
		and the number of minor unit digits provided by the [ISO 4217 currency code list](http://www.currency-iso.org/en/home/tables/table-a1.html)
		(2 if the list doesn't provide that information); the default for percent formatting is the larger of
		minimumFractionDigits and 0.
	**/
	var ?maximumFractionDigits:Int;

	/**
		The minimum number of significant digits to use.
		Possible values are from 1 to 21; the default is 1.
	**/
	var ?minimumSignificantDigits:Int;

	/**
		The maximum number of significant digits to use.
		Possible values are from 1 to 21; the default is 21.
	**/
	var ?maximumSignificantDigits:Int;
}

typedef NumberFormatResolvedOption = {
	/**
		The BCP 47 language tag for the locale actually used. If any Unicode extension values were
		requested in the input BCP 47 language tag that led to this locale, the key-value pairs that
		were requested and are supported for this locale are included in `locale`.
	**/
	final locale:String;

	/**
		The value requested using the Unicode extension key `"nu"` or filled in as a default.
	**/
	final numberingSystem:String;

	final style:NumberFormatStyle;

	/**
		The values provided for these properties in the `options` argument or filled in as defaults.
	**/
	final useGrouping:String;

	final currency:String;

	/**
		The values provided for these properties in the `options` argument or filled in as defaults.
		These properties are only present if `style` is `"currency"`.
	**/
	final currencyDisplay:String;

	final minimumIntegerDigits:Int;
	final minimumFractionDigits:Int;

	/**
		The values provided for these properties in the `options` argument or filled in as defaults.
		These properties are present only if neither m`inimumSignificantDigits` nor `maximumSignificantDigits`
		was provided in the `options` argument.
	**/
	final maximumFractionDigits:Int;

	final minimumSignificantDigits:Int;

	/**
		The values provided for these properties in the `options` argument or filled in as defaults.
		These properties are present only if at least one of them was provided in the `options` argument.
	**/
	final maximumSignificantDigits:Int;
}

enum abstract NumberFormatStyle(String) {
	/**
		plain number formatting
	**/
	var Decimal = "decimal";

	/**
		currency formatting
	**/
	var Currency = "currency";

	/**
		percent formatting
	**/
	var Percent = "percent";
}

enum abstract CurrencyDisplay(String) {
	/**
		To use a localized currency symbol such as €.
	**/
	var Symbol = "symbol";

	/**
		To use the ISO currency code.
	**/
	var Code = "code";

	/**
		To use a localized currency name such as "dollar".
	**/
	var Name = "name";
}

typedef NumberFormatPart = {
	final type:NumberFormatPartType;
	final value:String;
}

enum abstract NumberFormatPartType(String) {
	/**
		The currency string, such as the symbols "$" and "€" or the name "Dollar", "Euro" depending
		on how currencyDisplay is specified.
	**/
	var Currency = "currency";

	/**
		The decimal separator string (".").
	**/
	var Decimal = "decimal";

	/**
		The fraction number.
	**/
	var Fraction = "fraction";

	/**
		The group separator string (",").
	**/
	var group = "group";

	/**
		The Infinity string ("∞").
	**/
	var infinity = "infinity";

	/**
		The integer number.
	**/
	var integer = "integer";

	/**
		Any literal strings or whitespace in the formatted number.
	**/
	var literal = "literal";

	/**
		The minus sign string ("-").
	**/
	var minusSign = "minusSign";

	/**
		The NaN string ("NaN").
	**/
	var nan = "nan";

	/**
		The plus sign string ("+").
	**/
	var plusSign = "plusSign";

	/**
		The percent sign string ("%").
	**/
	var percentSign = "percentSign";
}

typedef NumberFormatSupportedLocalesOfOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	 */
	var ?localeMatcher:LocaleMatcher;
}
