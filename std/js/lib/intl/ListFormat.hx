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

package js.lib.intl;

/**
	The `Intl.ListFormat` object enables language-sensitive list formatting.
**/
@:native("Intl.ListFormat")
extern class ListFormat {

	/**
		Creates a new `Intl.ListFormat` object.
	**/
	@:overload(function(?locales: Array<String>, ?options: ListFormatOptions): Void {})
	function new(?locales: String, ?options: ListFormatOptions);

	/**
		Returns a language-specific formatted string representing the elements of the list.
	**/
	function format(?list: Array<String>): String;

	/**
		Returns an array of objects representing the different components
		that can be used to format a list of values in a locale-aware fashion.
	**/
	function formatToParts(?list: Array<String>): Array<ListFormatPart>;

	/**
		Returns an array containing those of the provided locales that are supported
		without having to fall back to the runtime's default locale.
	**/
	@:overload(function(locales: Array<String>, ?options: ListFormatSupportedLocalesOfOptions): Array<String> {})
	static function supportedLocalesOf(locales: String, ?options: ListFormatSupportedLocalesOfOptions): Array<String>;
}

typedef ListFormatOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	**/
	var ?localeMatcher: LocaleMatcher;

	/**
		The length of the formatted message.
		The default is `Long`.
	**/
	var ?style: ListFormatStyle;

	/**
		The format of output message.
	**/
	var ?type: ListFormatType;
}

typedef ListFormatPart = {
	final type: ListFormatPartType;
	final value: String;
}

enum abstract ListFormatPartType(String) {
	/**
		A value from the list.
	**/
	var Element = "element";

	/**
		A linguistic construct.
	**/
	var Literal = "literal";
}

enum abstract ListFormatStyle(String) {
	var Long = "long";
	var Narrow = "narrow";
	var Short = "short";
}

typedef ListFormatSupportedLocalesOfOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	 */
	var ?localeMatcher: LocaleMatcher;
}

enum abstract ListFormatType(String) {
	/**
		Stands for "and"-based lists.
	**/
	var Conjunction = "conjunction";

	/**
		Stands for "or"-based lists.
	**/
	var Disjunction = "disjunction";

	/**
		Stands for lists of values with units.
	**/
	var Unit = "unit";
}
