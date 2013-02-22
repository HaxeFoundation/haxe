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
import cs.StdTypes;

/**
	The basic String class.
**/
@:coreType extern class String implements ArrayAccess<Char16> {

	private static function Compare(s1:String, s2:String):Int;

	/**
		The number of characters in the String.
	**/
	var length(default,null) : Int;

	/**
		Creates a copy from a given String.
	**/
	function new(string:String) : Void;

	/**
		Returns an String where all characters have been uppercased.
	**/
	function toUpperCase() : String;

	/**
		Returns an String where all characters have been lowercased.
	**/
	function toLowerCase() : String;

	/**
		Returns the character at the given position.
		Returns the empty String if outside of String bounds.
	**/
	function charAt( index : Int) : String;

	/**
		Returns the character code at the given position.
		Returns [null] if outside of String bounds.
	**/
	function charCodeAt( index : Int) : Null<Int>;

	/**
		Returns the index of first occurence of [value]
		Returns [1-1] if [value] is not found.
		The optional [startIndex] parameter allows you to specify at which character to start searching.
		The position returned is still relative to the beginning of the string.
	**/
	function indexOf( str : String, ?startIndex : Int ) : Int;

	/**
		Similar to [indexOf] but returns the latest index.
	**/
	function lastIndexOf( str : String, ?startIndex : Int ) : Int;

	/**
		Split the string using the specified delimiter.
	**/
	function split( delimiter : String ) : Array<String>;

	/**
		Returns a part of the String, taking [len] characters starting from [pos].
		If [len] is not specified, it takes all the remaining characters.
	**/
	function substr( pos : Int, ?len : Int ) : String;

	/**
		Returns a part of the String, taking from [startIndex] to [endIndex] - 1.
		If [endIndex] is not specified, length is used.
		If [startIndex] or [endIndex] is smaller than 0, than 0 is used.
		If [startIndex] > [endIndex] then they are swaped.
	**/
	function substring( startIndex : Int, ?endIndex : Int ) : String;

	/**
		Returns the String itself.
	**/
	function toString() : String;

	static function fromCharCode( code : Int ) : String;

	private function Replace(oldValue:String, newValue:String):String;
	private function StartsWith(value:String):Bool;
	private function EndsWith(value:String):Bool;
	private function TrimStart():String;
	private function TrimEnd():String;
	private function Trim():String;
	private function CompareTo(obj:Dynamic):Int;
	@:overload(function(startIndex:Int):String {})
	private function Substring(startIndex:Int, length:Int):String;

}
