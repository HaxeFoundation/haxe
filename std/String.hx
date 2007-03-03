/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

/**
	The basic String class.
**/
extern class String {

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
	function indexOf( value : String, ?startIndex : Int ) : Int;

	/**
		Similar to [indexOf] but returns the latest index.
	**/
	function lastIndexOf( value : String, ?startIndex : Int ) : Int;

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
		Returns the String itself.
	**/
	function toString() : String;

	static function fromCharCode( code : Int ) : String;

}
