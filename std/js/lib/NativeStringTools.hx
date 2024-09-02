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

package js.lib;

import haxe.extern.EitherType;
import js.Lib.undefined;
import js.Syntax;
import js.lib.intl.Collator.CollatorOptions;

/**
	Documentation [String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
class NativeStringTools {
	/**
		Returns the Unicode Normalization Form of the calling string value.
	 */
	public static inline function normalize(string:String, form:NormalizationForm):String {
		return Syntax.code("{0}.normalize({1})", string, form);
	}

	/**
		Returns a number indicating whether a reference string comes before or after or is
		the same as the given string in sort order.
	 */
	public static inline function localeCompare(string:String, compareString:String, ?locales:EitherType<String, Array<String>>, ?options:CollatorOptions):Bool {
		return Syntax.code(
			"{0}.localeCompare({1}, {2}, {3})",
			string,
			(compareString != null) ? compareString : undefined,
			(locales != null) ? locales : undefined,
			(options != null) ? options : undefined
		);
	}

	/**
		The characters within a string are converted to lower case while respecting the current locale.
		For most languages, this will return the same as toLowerCase().
	 */
	public static inline function toLocaleLowerCase(string:String, ?locales:EitherType<String, Array<String>>):String {
		return Syntax.code(
			"{0}.toLocaleLowerCase({1})",
			string,
			(locales != null) ? locales : undefined
		);
	}

	/**
		The characters within a string are converted to upper case while respecting the current locale.
		For most languages, this will return the same as toUpperCase().
	 */
	public static inline function toLocaleUpperCase(string:String, ?locales:EitherType<String, Array<String>>):String {
		return Syntax.code(
			"{0}.toLocaleUpperCase({1})",
			string,
			(locales != null) ? locales : undefined
		);
	}
}

enum abstract NormalizationForm(String) {
	/**
		Normalization Form Canonical Composition.
	 */
	var NFC;

	/**
		Normalization Form Canonical Decomposition.
	 */
	var NFD;

	/**
		Normalization Form Compatibility Composition.
	 */
	var NFKC;

	/**
		Normalization Form Compatibility Decomposition.
	 */
	var NFKD;
}
