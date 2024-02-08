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
	The `RelativeTimeFormat` object is a constructor for objects that enable language-sensitive
	relative time formatting.

	Documentation [RelativeTimeFormat](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RelativeTimeFormat) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RelativeTimeFormat$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Intl.RelativeTimeFormat")
extern class RelativeTimeFormat {
	@:overload(function(?locales:Array<String>, ?options:RelativeTimeFormatOptions):Void {})
	@:pure function new(?locales:String, ?options:RelativeTimeFormatOptions);

	/**
		Formats a value and a unit according to the locale and formatting options
		of the given Intl.RelativeTimeFormat object.
	**/
	@:pure function format(value:Float, unit:RelativeTimeUnit):String;

	/**
		Returns an Array of objects representing the relative time format in parts
		that can be used for custom locale-aware formatting.
	**/
	@:pure function formatToParts(value:Float, unit:RelativeTimeUnit):Array<RelativeTimeFormatPart>;

	/**
		Returns a new object with properties reflecting the locale and formatting options
		computed during initialization of the object.
	**/
	@:pure function resolvedOptions():RelativeTimeFormatResolvedOptions;

	/**
		Returns an array containing those of the provided locales that are supported
		without having to fall back to the runtime's default locale.
	**/
	@:overload(function(locales:Array<String>, ?options:RelativeTimeFormatSupportedLocalesOfOptions):Array<String> {})
	@:pure static function supportedLocalesOf(locales:String, ?options:RelativeTimeFormatSupportedLocalesOfOptions):Array<String>;
}

typedef RelativeTimeFormatOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
		For information about this option, see [Intl](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl#Locale_negotiation).
	**/
	var ?localeMatcher:LocaleMatcher;

	/**
		The format of output message.
		The default value is `Always`.
		The `Auto` value allows to not always have to use numeric values in the output.
	**/
	var ?numeric:RelativeTimeNumeric;

	/**
		The length of the internationalized message.
		The default value is `Long`.
		The `Narrow` style could be similar to the short style for some locales.
	**/
	var ?style:RelativeTimeFormatStyle;
}

typedef RelativeTimeFormatResolvedOptions = {
	/**
		The BCP 47 language tag for the locale actually used. If any Unicode extension values were requested in
		the input BCP 47 language tag that led to this locale, the key-value pairs that were requested and are
		supported for this locale are included in `locale`.
	**/
	final locale:String;

	/**
		The length of the internationalized message.
	**/
	final style:RelativeTimeFormatStyle;

	/**
		The format of output message.
	**/
	final numeric:RelativeTimeNumeric;

	/**
		The value requested using the Unicode extension key `"nu"` or filled in as a default.
	**/
	final numberingSystem:String;
}

enum abstract RelativeTimeNumeric(String) {
	/**
		(e.g., 1 day ago),
	**/
	var Always = "always";

	/**
		(e.g., yesterday).
	**/
	var Auto = "auto";
}

enum abstract RelativeTimeFormatStyle(String) {
	/**
		(e.g., in 1 month)
	 */
	var Long = "long";

	/**
		(e.g., in 1 mo.)
	 */
	var Short = "short";

	/**
		(e.g., in 1 mo.)
	 */
	var Narrow = "narrow";
}

enum abstract RelativeTimeUnit(String) from String to String {
	var Year = "year";
	var Quarter = "quarter";
	var Month = "month";
	var Week = "week";
	var Day = "day";
	var Hour = "hour";
	var Minute = "minute";
	var Second = "second";
}

typedef RelativeTimeFormatPart = {
	final type:RelativeTimeFormatPartType;
	final value:String;
	final ?unit:RelativeTimeUnit;
}

typedef RelativeTimeFormatPartType = NumberFormat.NumberFormatPartType;

typedef RelativeTimeFormatSupportedLocalesOfOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	 */
	var ?localeMatcher:LocaleMatcher;
}
