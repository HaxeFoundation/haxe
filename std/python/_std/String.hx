package;
/*
 * Copyright (C)2005-2012 Haxe Foundation
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

	A haxe String is immutable, it is not possible to modify individual
	characters. No method of this class changes the state of [this] String.

	Strings can be constructed using the string literal syntax "string value".

	String can be concatenated by using the + operator. If an operand is not a
	String, it is passed through Std.string() first.
**/
#if !macro
import python.internal.StringImpl;
#end
@:pythonImport("builtins", "str")
@:coreApi
extern class String {


	/**
		The number of characters in [this] String.
	**/
	var length(default,null) : Int;

	private inline function get_length ():Int {
		return StringImpl.get_length(this);
	}

	/**
		Creates a copy from a given String.
	**/
	function new(string:String) : Void;

	/**
		Returns a String where all characters of [this] String are upper case.

		Affects the characters [a-z]. Other characters remain unchanged.
	**/
	@:runtime public inline function toUpperCase() : String {
		return StringImpl.toUpperCase(this);
	}

	/**
		Returns a String where all characters of [this] String are lower case.

		Affects the characters [A-Z]. Other characters remain unchanged.
	**/
	@:runtime public inline function toLowerCase() : String {
		return StringImpl.toLowerCase(this);
	}

	/**
		Returns the character at position [index] of [this] String.

		If [index] is negative or exceeds [this].length, the empty String ""
		is returned.
	**/
	inline public function charAt(index : Int) : String
	{
		return StringImpl.charAt(this, index);
	}

	/**
		Returns the character code at position [index] of [this] String.

		If [index] is negative or exceeds [this].length, null is returned.

		To obtain the character code of a single character, "x".code can be used
		instead to inline the character code at compile time. Note that this
		only works on String literals of length 1.
	**/
	inline public function charCodeAt( index : Int) : Null<Int>
	{
		return StringImpl.charCodeAt(this, index);
	}

	/**
		Returns the position of the leftmost occurence of [str] within [this]
		String.

		If [startIndex] is given, the search is performed within the substring
		of [this] String starting from [startIndex]. Otherwise the search is
		performed within [this] String. In either case, the returned position
		is relative to the beginning of [this] String.

		If [str] cannot be found, -1 is returned.
	**/
	inline function indexOf( str : String, ?startIndex : Int ) : Int {
		return StringImpl.indexOf(this, str, startIndex);
	}

	/**
		Returns the position of the rightmost occurence of [str] within [this]
		String.

		If [startIndex] is given, the search is performed within the substring
		of [this] String from 0 to [startIndex]. Otherwise the search is
		performed within [this] String. In either case, the returned position
		is relative to the beginning of [this] String.

		If [str] cannot be found, -1 is returned.
	**/
	inline function lastIndexOf( str : String, ?startIndex : Int ) : Int {
		return StringImpl.lastIndexOf(this, str, startIndex);
	}

	/**
		Splits [this] String at each occurence of [delimiter].

		If [delimiter] is the empty String "", [this] String is split into an
		Array of [this].length elements, where the elements correspond to the
		characters of [this] String.

		If [delimiter] is not found within [this] String, the result is an Array
		with one element, which equals [this] String.

		If [delimiter] is null, the result is unspecified.

		Otherwise, [this] String is split into parts at each occurence of
		[delimiter]. If [this] String starts (or ends) with [delimiter}, the
		result Array contains a leading (or trailing) empty String "" element.
		Two subsequent delimiters also result in an empty String "" element.
	**/
	inline function split( delimiter : String ) : Array<String> {
		return StringImpl.split(this, delimiter);
	}

	/**
		Returns [len] characters of [this] String, starting at position [pos].

		If [len] is omitted, all characters from position [pos] to the end of
		[this] String are included.

		If [pos] is negative, its value is calculated from the end of [this]
		String by [this].length + [pos]. If this yields a negative value, 0 is
		used instead.

		If the calculated position + [len] exceeds [this].length, the characters
		from that position to the end of [this] String are returned.

		If [len] is negative, the result is unspecified.
	**/
	inline public function substr( pos : Int, ?len : Int ) : String
	{
		return StringImpl.substr(this, pos, len);
	}

	/**
		Returns the part of [this] String from [startIndex] to [endIndex].

		If [startIndex] or [endIndex] are negative, 0 is used instead.

		If [startIndex] exceeds [endIndex], they are swapped.

		If the (possibly swapped) [endIndex] is omitted or exceeds
		[this].length, [this].length is used instead.

		If the (possibly swapped) [startIndex] exceeds [this].length, the empty
		String "" is returned.
	**/
	inline function substring( startIndex : Int, ?endIndex : Int ) : String {
		return StringImpl.substring(this, startIndex, endIndex);
	}

	/**
		Returns the String itself.
	**/
	inline function toString() : String return StringImpl.toString(this);

	/**
		Returns the String corresponding to the character code [code].

		If [code] is negative or has another invalid value, the result is
		unspecified.
	**/
	public static inline function fromCharCode( code : Int ) : String {
		return StringImpl.fromCharCode(code);
	}
}
