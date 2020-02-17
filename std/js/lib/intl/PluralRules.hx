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
	The `PluralRules` object is a constructor for objects that enable plural sensitive formatting
	and plural language rules.

	Documentation [PluralRules](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/PluralRules) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/PluralRules$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Intl.PluralRules")
extern class PluralRules {
	@:overload(function(?locales:Array<String>, ?options:PluralRulesOptions):Void {})
	@:pure function new(?locales:String, ?options:PluralRulesOptions);

	/**
		Returns a new object with properties reflecting the locale and collation options computed during initialization of the object.
	**/
	@:pure function resolvedOptions():PluralRulesResolvedOptions;

	/**
		Returns a String indicating which plurar rule to use for locale-aware formatting.
	**/
	@:pure function select(number:Int):String;

	/**
		Returns an array containing those of the provided locales that are supported
		without having to fall back to the runtime's default locale.
	**/
	@:overload(function(locales:Array<String>, ?options:PluralRulesSupportedLocalesOfOptions):Array<String> {})
	@:pure static function supportedLocalesOf(locales:String, ?options:PluralRulesSupportedLocalesOfOptions):Array<String>;
}

typedef PluralRulesOptions = {
	/**
		The locale matching algorithm to use.
		The default is "best fit".
		For information about this option, see the [Intl page](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl#Locale_negotiation).
	 */
	var ?localeMatcher:LocaleMatcher;

	/**
		The type to use.
		The default is `Cardinal`.
	 */
	var ?type:PluralRulesType;
}

typedef PluralRulesResolvedOptions = {
	/**
		The BCP 47 language tag for the locale actually used. If any Unicode extension values were requested in
		the input BCP 47 language tag that led to this locale, the key-value pairs that were requested and are
		supported for this locale are included in `locale`.
	 */
	final locale:String;

	/**
		An `Array` of plural rules used by the given language.
	**/
	final pluralCategories:Array<String>;

	/**
		The type used (cardinal or ordinal).
	**/
	final type:PluralRulesType;

	final minimumIntegerDigits:Int;
	final minimumFractionDigits:Int;

	/**
		The values provided for these properties in the `options` argument or filled in as defaults.
		These properties are present only if neither `minimumSignificantDigits` nor `maximumSignificantDigits`
		was provided in the options argument.
	**/
	final maximumFractionDigits:Int;

	final minimumSignificantDigits:Int;

	/**
		The values provided for these properties in the `options` argument or filled in as defaults.
		These properties are present only if at least one of them was provided in the `options` argument.
	**/
	final maximumSignificantDigits:Int;
}

enum abstract PluralRulesType(String) {
	/**
		For cardinal numbers (refering to the quantity of things).
	 */
	var Cardinal = "cardinal";

	/**
		For ordinal number (refering to the ordering or ranking of things, e.g. "1st", "2nd", "3rd" in English).
	 */
	var Ordinal = "ordinal";
}

typedef PluralRulesSupportedLocalesOfOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	 */
	var ?localeMatcher:LocaleMatcher;
}
