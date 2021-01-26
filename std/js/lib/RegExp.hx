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

import haxe.DynamicAccess;

/**
	Native JavaScript regular expressions.

	For cross-platform regular expressions, use Haxe `EReg` class or
	[regexp literals](https://haxe.org/manual/std-regex.html).

	@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp>
**/
@:native("RegExp")
extern class RegExp {
	/**
		Indicates whether or not the "g" flag is used with the regular expression.
	**/
	var global(default, null):Bool;

	/**
		Indicates whether or not the "i" flag is used with the regular expression.
	**/
	var ignoreCase(default, null):Bool;

	/**
		Indicates whether or not the "m" flag is used with the regular expression.
	**/
	var multiline(default, null):Bool;

	/**
		The source text of the regexp object, it doesn't contain the two forward slashes on both sides and any flags.
	**/
	var source(default, null):String;

	/**
		The index at which to start the next match.
	**/
	var lastIndex:Int;

	/**
		Create a regular expression object for matching text with a pattern.
	**/
	function new(pattern:String, ?flags:String);

	/**
		Execute a search for a match in a specified string.
		Returns a result array, or null.
	**/
	function exec(str:String):Null<RegExpMatch>;

	/**
		Execute a search for a match between a regular expression and a specified string.
		Returns true or false.
	**/
	function test(str:String):Bool;

	/**
		Return a string representing the regular expression.
	**/
	function toString():String;
}

/**
	A return value of the `RegExp.exec` method.
**/
extern class RegExpMatch extends Array<String> {
	/**
		The index of the search at which the result was found.
	**/
	var index:Int;

	/**
		A copy of the search string.
	**/
	var input:String;

	/**
		Named capturing groups or undefined if no named capturing groups were defined.
		See [Groups and Ranges](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Groups_and_Ranges) for more information.

		Note: Not all browsers support this feature; refer to the [compatibility table](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#Browser_compatibility).
	**/
	var groups:Null<DynamicAccess<String>>;
}
