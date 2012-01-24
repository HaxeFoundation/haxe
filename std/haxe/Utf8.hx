/*
 * Copyright (c) 2005-2012, The haXe Project Contributors
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
package haxe;

/**
	Since all platforms does not guarantee that String always uses UTF-8 encoding, you
	can use this crossplatform API to perform operations on such strings.
**/
class Utf8 {

	var __b : String;

	/**
		Allocate a new Utf8 buffer using an optional bytes size.
	**/
	public function new( ?size : Int ) {
		__b = "";
	}

	/**
		Add the given UTF8 character code to the buffer.
	**/
	public inline function addChar( c : Int ) : Void {
		__b += String.fromCharCode(c);
	}

	/**
		Returns the buffer converted to a String;
	**/
	public inline function toString() : String {
		return __b;
	}

	/**
		Call the [chars] function for each UTF8 char of the string.
	**/
	public static function iter( s : String, chars : Int -> Void ) {
		for( i in 0...s.length )
			chars(s.charCodeAt(i));
	}

	/**
		Encode the input ISO string into the corresponding UTF8 one.
	**/
	public static function encode( s : String ) : String {
		throw "Not implemented";
		return s;
	}

	/**
		Decode an UTF8 string back to an ISO string.
		Throw an exception if a given UTF8 character is not supported by the decoder.
	**/
	public static function decode( s : String ) : String {
		throw "Not implemented";
		return s;
	}

	/**
		Similar to [String.charCodeAt] but uses the UTF8 character position.
	**/
	public static inline function charCodeAt( s : String, index : Int ) : Int {
		return s.charCodeAt(index);
	}

	/**
		Tells if the String is correctly encoded as UTF8.
	**/
	public static inline function validate( s : String ) : Bool {
		return true;
	}

	/**
		Returns the number of UTF8 chars of the String.
	**/
	public static inline function length( s : String ) : Int {
		return s.length;
	}

	/**
		Compare two UTF8 strings, character by character.
	**/
	public static function compare( a : String, b : String ) : Int {
		return a > b ? 1 : (a == b ? 0 : -1);
	}

	/**
		This is similar to [String.substr] but the [pos] and [len] parts are considering UTF8 characters.
	**/
	public static inline function sub( s : String, pos : Int, len : Int ) : String {
		return s.substr(pos,len);
	}

}