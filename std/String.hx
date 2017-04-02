/*
 * Copyright (C)2005-2017 Haxe Foundation
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
/**
	The basic String class.

	A Haxe String is immutable, it is not possible to modify individual
	characters. No method of this class changes the state of `this` String.

	Strings can be constructed using the String literal syntax `"string value"`.

	String can be concatenated by using the `+` operator. If an operand is not a
	String, it is passed through `Std.string()` first.
	
	@see https://haxe.org/manual/std-String.html
**/
extern class String {

	/**
		The number of characters in `this` String.
	**/
	var length(default,null) : Int;

	/**
		Creates a copy from a given String.
	**/
	function new(string:String) : Void;

	/**
		Returns a String where all characters of `this` String are upper case.

		Affects the characters `a-z`. Other characters remain unchanged.
	**/
	function toUpperCase() : String;

	/**
		Returns a String where all characters of `this` String are lower case.

		Affects the characters `A-Z`. Other characters remain unchanged.
	**/
	function toLowerCase() : String;

	/**
		Returns the character at position `index` of `this` String.

		If `index` is negative or exceeds `this.length`, the empty String `""`
		is returned.
	**/
	function charAt(index : Int) : String;

	/**
		Returns the character code at position `index` of `this` String.

		If `index` is negative or exceeds `this.length`, `null` is returned.

		To obtain the character code of a single character, `"x".code` can be
		used instead to inline the character code at compile time. Note that
		this only works on String literals of length 1.
	**/
	function charCodeAt( index : Int) : Null<Int>;

	/**
		Returns the position of the leftmost occurrence of `str` within `this`
		String.

		If `startIndex` is given, the search is performed within the substring
		of `this` String starting from `startIndex`. Otherwise the search is
		performed within `this` String. In either case, the returned position
		is relative to the beginning of `this` String.

		If `str` cannot be found, -1 is returned.
	**/
	function indexOf( str : String, ?startIndex : Int ) : Int;

	/**
		Returns the position of the rightmost occurrence of `str` within `this`
		String.

		If `startIndex` is given, the search is performed within the substring
		of `this` String from 0 to `startIndex`. Otherwise the search is
		performed within `this` String. In either case, the returned position
		is relative to the beginning of `this` String.

		If `str` cannot be found, -1 is returned.
	**/
	function lastIndexOf( str : String, ?startIndex : Int ) : Int;

	/**
		Splits `this` String at each occurrence of `delimiter`.

		If `this` String is the empty String `""`, the result is not consistent
		across targets and may either be `[]` (on Js, Cpp) or `[""]`.

		If `delimiter` is the empty String `""`, `this` String is split into an
		Array of `this.length` elements, where the elements correspond to the
		characters of `this` String.

		If `delimiter` is not found within `this` String, the result is an Array
		with one element, which equals `this` String.

		If `delimiter` is null, the result is unspecified.

		Otherwise, `this` String is split into parts at each occurrence of
		`delimiter`. If `this` String starts (or ends) with `delimiter`, the
		result `Array` contains a leading (or trailing) empty String `""` element.
		Two subsequent delimiters also result in an empty String `""` element.
	**/
	function split( delimiter : String ) : Array<String>;

	/**
		Returns `len` characters of `this` String, starting at position `pos`.

		If `len` is omitted, all characters from position `pos` to the end of
		`this` String are included.

		If `pos` is negative, its value is calculated from the end of `this`
		String by `this.length + pos`. If this yields a negative value, 0 is
		used instead.

		If the calculated position + `len` exceeds `this.length`, the characters
		from that position to the end of `this` String are returned.

		If `len` is negative, the result is unspecified.
	**/
	function substr( pos : Int, ?len : Int ) : String;

	/**
		Returns the part of `this` String from `startIndex` to but not including `endIndex`.

		If `startIndex` or `endIndex` are negative, 0 is used instead.

		If `startIndex` exceeds `endIndex`, they are swapped.

		If the (possibly swapped) `endIndex` is omitted or exceeds
		`this.length`, `this.length` is used instead.

		If the (possibly swapped) `startIndex` exceeds `this.length`, the empty
		String `""` is returned.
	**/
	function substring( startIndex : Int, ?endIndex : Int ) : String;

	/**
		Returns the String itself.
	**/
	function toString() : String;

	/**
		Returns the String corresponding to the character code `code`.

		If `code` is negative or has another invalid value, the result is
		unspecified.
	**/
	@:pure static function fromCharCode( code : Int ) : String;
}
