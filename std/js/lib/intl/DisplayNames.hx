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
	The `Intl.DisplayNames` object enables the consistent translation of language,
	region and script display names.
**/
@:native("Intl.DisplayNames")
extern class DisplayNames {

	/**
		Creates a new `Intl.DisplayNames` object.
	**/
	@:overload(function(?locales: Array<String>, ?options: DisplayNamesOptions): Void {})
	function new(?locales: String, ?options: DisplayNamesOptions);

	/**
		Receives a code and returns a string based on the locale and options
		provided when instantiating `Intl.DisplayNames`.
	**/
	function of(code: String): String;

	/**
		Returns a new object with properties reflecting the locale and formatting options
		computed during initialization of the object.
	**/
	function resolvedOptions(): DisplayNamesResolvedOptions;

	/**
		Returns an array containing those of the provided locales that are supported
		in display names without having to fall back to the runtime's default locale.
	**/
	@:overload(function(locales: Array<String>, ?options: DisplayNamesSupportedLocalesOfOptions): Array<String> {})
	static function supportedLocalesOf(locales: String, ?options: DisplayNamesSupportedLocalesOfOptions): Array<String>;
}

enum abstract DisplayNamesFallback(String) {
	var Code = "code";
	var None = "none";
}

typedef DisplayNamesOptions = {
	/**
		The fallback to use.
		The default is `Code`.
	**/
	var fallback: DisplayNamesFallback;

	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	**/
	var ?localeMatcher: LocaleMatcher;

	/**
		The formatting style to use.
		The default is `Long`.
	**/
	var ?style: DisplayNamesStyle;

	/**
		The type to use.
		The default is `Language`.
	**/
	var ?type: DisplayNamesType;
}

typedef DisplayNamesResolvedOptions = {
	/**
		The value provided for this property in the `options` argument of the constructor
		or the default value (`Code`).
	**/
	final fallback: DisplayNamesFallback;

	/**
		The BCP 47 language tag for the locale actually used.
	**/
	final locale: String;

	/**
		The value provided for this property in the `options` argument of the constructor
		or the default value (`Long`).
	**/
	final style: DisplayNamesStyle;

	/**
		The value provided for this property in the `options` argument of the constructor
		or the default value (`Language`).
	**/
	final type: DisplayNamesType;
}

enum abstract DisplayNamesStyle(String) {
	var Long = "long";
	var Narrow = "narrow";
	var Short = "short";
}

typedef DisplayNamesSupportedLocalesOfOptions = {
	/**
		The locale matching algorithm to use.
		The default is `BestFit`.
	 */
	var ?localeMatcher: LocaleMatcher;
}

enum abstract DisplayNamesType(String) {
	var Currency = "currency";
	var Language = "language";
	var Region = "region";
	var Script = "script";
}
