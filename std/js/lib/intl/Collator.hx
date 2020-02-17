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
	The `Collator` object is a constructor for collators, objects that enable language
	sensitive string comparison.

	Documentation [Collator](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Collator) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Collator$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Intl.Collator")
extern class Collator {
	@:overload(function(?locales:Array<String>, ?options:CollatorOptions):Void {})
	@:pure function new(?locales:String, ?options:CollatorOptions);

	/**
		Getter function that compares two strings according to the sort order of this `Collator` object.
	 */
	@:pure function compare(string1:String, string2:String):Int;

	/**
		Returns a new object with properties reflecting the locale and collation options computed
		during initialization of the object.
	 */
	@:pure function resolvedOptions():CollatorResolvedOptions;

	/**
		Returns an array containing those of the provided locales that are supported
		without having to fall back to the runtime's default locale.
		@param locales A string with a BCP 47 language tag, or an array of such strings.
	**/
	@:overload(function(locales:Array<String>, ?options:CollatorSupportedLocalesOfOptions):Array<String> {})
	@:pure static function supportedLocalesOf(locales:String, ?options:CollatorSupportedLocalesOfOptions):Array<String>;
}

typedef CollatorOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
		For information about this option, see the [Intl page](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl#Locale_negotiation).
	**/
	var ?localeMatcher:LocaleMatcher;

	/**
		Whether the comparison is for sorting or for searching for matching strings.
		The default is `Sort`.
	**/
	var ?usage:Usage;

	/**
		Which differences in the strings should lead to non-zero result values.
		The default is `Variant` for usage `Sort`; it's locale dependent for usage `Search`.
	**/
	var ?sensitivity:Sensitivity;

	/**
		Whether punctuation should be ignored.
		The default is `false`.
	**/
	var ?ignorePunctuation:Bool;

	/**
		Whether numeric collation should be used, such that "1" < "2" < "10".
		The default is `false`.
		This option can be set through an `options` property or through a Unicode extension key;
		if both are provided, the `options` property takes precedence.
		Implementations are not required to support this property.
	**/
	var ?numeric:Bool;

	/**
		Whether upper case or lower case should sort first.
		The default is "false".
		This option can be set through an options property or through a Unicode extension key;
		if both are provided, the `options` property takes precedence.
		Implementations are not required to support this property.
	**/
	var ?caseFirst:String;
}

typedef CollatorResolvedOptions = {
	/**
		The BCP 47 language tag for the locale actually used.
		If any Unicode extension values were requested in the input BCP 47 language tag
		that led to this locale, the key-value pairs that were requested and are supported
		for this locale are included in `locale`.
	**/
	final locale:String;

	final usage:Usage;
	final sensitivity:Sensitivity;

	/**
		The values provided for these properties in the `options` argument or filled in as defaults.
	**/
	final ignorePunctuation:Bool;

	/**
		he value requested using the Unicode extension key `"co"`, if it is supported for `Locale`,
		or `Default`.
	**/
	final collation:Collation;

	final numeric:Bool;

	/**
		The values requested for these properties in the options argument or using the
		Unicode extension keys `"kn"` and `"kf"` or filled in as defaults.
		If the implementation does not support these properties, they are omitted.
	**/
	final caseFirst:CaseFirst;
}

enum abstract Usage(String) {
	var Sort = "sort";
	var Search = "search";
}

enum abstract Sensitivity(String) {
	/**
		Only strings that differ in base letters compare as unequal.
		Examples: a ≠ b, a = á, a = A.
	**/
	var Base = "base";

	/**
		Only strings that differ in base letters or accents and other diacritic marks compare as unequal.
		Examples: a ≠ b, a ≠ á, a = A.
	**/
	var Accent = "accent";

	/**
		Only strings that differ in base letters or case compare as unequal.
		Examples: a ≠ b, a = á, a ≠ A.
	**/
	var Case = "case";

	/**
		Strings that differ in base letters, accents and other diacritic marks, or case compare as unequal.
		Other differences may also be taken into consideration.
		Examples: a ≠ b, a ≠ á, a ≠ A.
	**/
	var Variant = "variant";
}

enum abstract CaseFirst(String) {
	var Upper = "upper";
	var Lower = "lower";
	var False = "false";
}

enum abstract Collation(String) {
	var Locale = "locale";
	var Default = "default";
}

typedef CollatorSupportedLocalesOfOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	 */
	var ?localeMatcher:LocaleMatcher;
}
