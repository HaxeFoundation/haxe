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
package haxe.i18n;

import haxe.i18n.Tools.NativeStringTools;

import haxe.i18n.Tools.Convert;

import haxe.i18n.Tools.StringBufTools;

class Utf32Iterator {
	var s:Utf32;
	var p:Int;

	public inline function new (s) {
		this.p = 0;
		this.s = s;
	}

	public inline function hasNext ():Bool {
		return p < s.length;
	}

	public inline function next ():Int {
		return s.fastCodeAt(p++);
	}
}

@:allow(haxe.i18n)
abstract Utf32(String) {

	public var length(get,never) : Int;

	public inline function new(str:String) : Void {
		this = str;
	}

	public inline function iterator () {
		return new Utf32Iterator(fromImpl(this));
	}

	inline function get_length():Int {
		return this.length;
	}

	public inline function toUpperCase() : Utf32 {
		return new Utf32(this.toUpperCase());
	}

	public inline function toLowerCase() : Utf32 {
		return new Utf32(this.toLowerCase());
	}

	public inline function charAt(index : Int) : Utf32 {
		return new Utf32(this.charAt(index));
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	public inline function fastCodeAt( index : Int) : Int {
		return StringTools.fastCodeAt(this, index);
	}

	public inline function indexOf( str : Utf32, ?startIndex : Int ) : Int {
		return this.indexOf(str.impl(),startIndex);
	}

	public inline function lastIndexOf( str : Utf32, ?startIndex : Int ) : Int {
		if (startIndex == null) { // required for flash
			return this.lastIndexOf(str.impl());
		}
		return this.lastIndexOf(str.impl(), startIndex);
	}

	public inline function split( delimiter : Utf32 ) : Array<Utf32> {
		return cast this.split(delimiter.impl());
	}

	public function substr( pos : Int, ?len : Int ) : Utf32 {
		if (len == null) return new Utf32(this.substr(pos)); // required for flash
		return new Utf32(this.substr(pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Utf32 {
		if (endIndex == null) return new Utf32(this.substring(startIndex));
		return new Utf32(this.substring(startIndex,endIndex));
	}

	public inline function toNativeString() : String {
		return this;
	}

	public static inline function fromCharCode( code : Int ) : Utf32 {
		return new Utf32(nativeStringfromCharCode(code));
	}

	static inline function nativeStringfromCharCode( code : Int ) : String {
		return String.fromCharCode(code);
	}

	public function toBytes(  ) : haxe.io.Bytes {
		var res = ByteAccess.alloc(this.length << 2);
		for (i in 0...this.length) {
			res.setInt32LE(i << 2, fastCodeAt(i));
		}
		return res.toBytes();
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf32 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public static function fromByteAccess (ba:ByteAccess):Utf32 {
		return new Utf32(ba.getData().decode("utf-32le"));
	}

	@:op(A == B) inline function opEq (other:Utf32) {
		return this == other.impl();
	}

	@:op(A + B) inline function opAdd (other:Utf32) {
		return fromImpl(this + other.impl());
	}

	@:op(A != B) inline function opNotEq (other:Utf32) {
		return !opEq(other);
	}

	public inline function toUtf8() : Utf8 {
		return Utf8.fromUtf32(fromImpl(this));
	}

	public inline function toUtf16() : Utf16 {
		return Utf16.fromUtf32(fromImpl(this));
	}

	public inline function toUcs2() : Ucs2 {
		return Ucs2.fromUtf32(fromImpl(this));
	}

	inline static function fromUcs2 (s:Ucs2):Utf32 {
		var buf = new StringBuf();
		s.eachCode(function (codePoint) StringBufTools.addString(buf, String.fromCharCode(codePoint)));
		return fromImpl(buf.toString());
	}

	inline static function fromUtf16 (s:Utf16):Utf32 {
		var buf = new StringBuf();
		s.eachCode(function (codePoint) StringBufTools.addString(buf, String.fromCharCode(codePoint)));
		return fromImpl(buf.toString());
	}

	inline static function fromUtf8 (s:Utf8):Utf32 {
		var buf = new StringBuf();
		s.eachCode(function (codePoint) StringBufTools.addString(buf, String.fromCharCode(codePoint)));
		return fromImpl(buf.toString());
	}

	static inline function fromImpl (str:String):Utf32 {
		return cast str;
	}

	inline function impl() : String {
		return this;
	}

	inline function eachCode ( f : Int -> Void) {
		for (c in fromImpl(this)) f(c);
	}

	public inline function getReader ():Utf32Reader {
		return new Utf32Reader(this);
	}

	public function toCodeArray ():Array<Int> {
		var res = [];
		eachCode(function (c) res.push(c));
		return res;
	}

	@:op(A > B) inline function opGreaterThan (other:Utf32) {
		return this > other.impl();
	}

	@:op(A < B) inline function opLessThan (other:Utf32) {
		return this < other.impl();
	}

	@:op(A <= B) inline function opLessThanOrEq (other:Utf32) {
		return this <= other.impl();
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf32) {
		return this >= other.impl();
	}
}

abstract Utf32Reader(String) {
	public inline function new (s:String) {
		this = s;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length;
	}

	public inline function getInt32 (pos:Int) {
		return StringTools.fastCodeAt(this, pos);
	}
}