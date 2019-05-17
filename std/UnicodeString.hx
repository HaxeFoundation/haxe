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

import haxe.iterators.StringIteratorUnicode;
import haxe.iterators.StringKeyValueIteratorUnicode;

/**
	This abstract provides consistent cross-target unicode support.

	@see https://haxe.org/manual/std-UnicodeString.html
**/
@:forward
@:access(StringTools)
abstract UnicodeString(String) from String to String {

	/**
		The number of characters in `this` String.
	**/
	public var length(get,never):Int;

	/**
		Creates an instance of UnicodeString.
	**/
	public inline function new(string:String):Void {
		this = string;
	}

	/**
		Returns the character at position `index` of `this` String.

		If `index` is negative or exceeds `this.length`, the empty String `""`
		is returned.
	**/
	#if !utf16 inline #end
	public function charAt(index:Int):String {
		#if utf16
			if(index < 0) return '';
			var unicodeOffset = 0;
			var nativeOffset = 0;
			while(nativeOffset < this.length) {
				var c = StringTools.utf16CodePointAt(this, nativeOffset++);
				if(unicodeOffset == index) {
					return String.fromCharCode(c);
				}
				if(c >= StringTools.MIN_SURROGATE_CODE_POINT) {
					nativeOffset++;
				}
				unicodeOffset++;
			}
			return '';
		#else
			return this.charAt(index);
		#end
	}

	/**
		Returns the character code at position `index` of `this` String.

		If `index` is negative or exceeds `this.length`, `null` is returned.
	**/
	#if !utf16 inline #end
	public function charCodeAt(index:Int):Null<Int> {
		#if utf16
			if(index < 0) return null;
			var unicodeOffset = 0;
			var nativeOffset = 0;
			while(nativeOffset < this.length) {
				var c = StringTools.utf16CodePointAt(this, nativeOffset++);
				if(unicodeOffset == index) {
					return c;
				}
				if(c >= StringTools.MIN_SURROGATE_CODE_POINT) {
					nativeOffset++;
				}
				unicodeOffset++;
			}
			return null;
		#else
			return this.charCodeAt(index);
		#end
	}

	/**
		Returns an iterator of the unicode code points.
	**/
	public inline function iterator():StringIteratorUnicode {
		return new StringIteratorUnicode(this);
	}

	/**
		Returns an iterator of the code point indices and unicode code points.
	**/
	public inline function keyValueIterator():StringKeyValueIteratorUnicode {
		return new StringKeyValueIteratorUnicode(this);
	}

	@:op(A < B) static function lt(a:UnicodeString, b:UnicodeString):Bool;
	@:op(A <= B) static function lte(a:UnicodeString, b:UnicodeString):Bool;
	@:op(A > B) static function gt(a:UnicodeString, b:UnicodeString):Bool;
	@:op(A >= B) static function gte(a:UnicodeString, b:UnicodeString):Bool;
	@:op(A == B) static function eq(a:UnicodeString, b:UnicodeString):Bool;
	@:op(A != B) static function neq(a:UnicodeString, b:UnicodeString):Bool;
	@:op(A + B) static function add(a:UnicodeString, b:UnicodeString):UnicodeString;
	@:op(A += B) static function assignAdd(a:UnicodeString, b:UnicodeString):UnicodeString;

	@:op(A + B) @:commutative static function add(a:UnicodeString, b:String):UnicodeString;
	@:op(A += B) @:commutative static function assignAdd(a:UnicodeString, b:String):UnicodeString;

	#if !utf16 inline #end
	function get_length():Int {
		#if utf16
			var l = 0;
			for(c in new StringIteratorUnicode(this)) {
				l++;
			}
			return l;
		#else
			return this.length;
		#end
	}
}
