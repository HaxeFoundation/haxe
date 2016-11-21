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

/**
	Cross platform UCS2 string API.
**/
#if (flash || js || hl || java || cs)

@:allow(haxe.i18n)
abstract Ucs2(String) {

	public var length(get,never) : Int;

	public inline function new(str:String) : Void {
		#if (java || js || cs || flash)
		// problem, java,js,java,cs have ucs2 apis but, the underlying string can actually
		// contain utf16 characters (invalid in ucs2), should we validate them at this point?	
		#end
		
		this = str;
	}

	inline function get_length():Int {
		return this.length;
	}

	public inline function toUpperCase() : Ucs2 {
		return new Ucs2(this.toUpperCase());
	}

	public inline function toLowerCase() : Ucs2 {
		return new Ucs2(this.toLowerCase());
	}

	public inline function charAt(index : Int) : Ucs2 {
		return new Ucs2(this.charAt(index));
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	public inline function fastCodeAt( index : Int) : Int {
		return StringTools.fastCodeAt(this, index);
	}

	public inline function indexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return this.indexOf(str.impl(),startIndex);
	}

	public inline function lastIndexOf( str : Ucs2, ?startIndex : Int ) : Int {
		if (startIndex == null) { // required for flash
			return this.lastIndexOf(str.impl());
		}
		return this.lastIndexOf(str.impl(), startIndex);
		
	}

	public inline function split( delimiter : Ucs2 ) : Array<Ucs2> {
		return cast this.split(delimiter.impl());
	}

	public function substr( pos : Int, ?len : Int ) : Ucs2 {
		if (len == null) return new Ucs2(this.substr(pos)); // required for flash
		return new Ucs2(this.substr(pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Ucs2 {
		if (endIndex == null) return new Ucs2(this.substring(startIndex));
		return new Ucs2(this.substring(startIndex,endIndex));
	}
	
	public inline function toNativeString() : String {
		return this;
	}

	public static inline function fromCharCode( code : Int ) : Ucs2 {
		return new Ucs2(String.fromCharCode(code));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		var b = haxe.io.Bytes.alloc(length*2);
		for (i in 0...length) {
			var code = charCodeAt(i);
			if (code == null) throw "assert";
			b.set(i * 2, ((code & 0xFF00) >> 8));
			b.set(i * 2 + 1, (code & 0x00FF));
		}
		return b;
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public static function fromByteAccess(bytes:ByteAccess):Ucs2 {
		var i = 0;
		var res = "";
		while (i < bytes.length) {
			var code = (bytes.fastGet(i) << 8) | bytes.fastGet(i+1);
			res += String.fromCharCode(code);
			i+=2;
		}
		return new Ucs2(res);
	}

	@:op(A == B) inline function opEq (other:Ucs2) {
		return this == other.impl();
	}

	@:op(A + B) inline function opAdd (other:Ucs2) {
		return fromImpl(this + other.impl());
	}

	@:op(A != B) inline function opNotEq (other:Ucs2) {
		return !opEq(other);
	}

	public inline function toUtf8() : Utf8 {
		return Utf8.fromByteAccess(Encoding.convertUcs2toUtf8(getReader(), StrictConversion));
	}

	public inline function toUtf16() : Utf16 {
		return Utf16.fromBytes(toBytes());
	}

	static inline function fromImpl (str:String):Ucs2 {
		return cast str;
	}
	inline function impl() : String {
		return this;
	}

	inline function eachCode ( f : Int -> Void) {
		for (i in 0...length) {
			var code = fastCodeAt(i);
			f(code);
		}
	}

	public function getReader ():Ucs2Reader {
		return new Ucs2Reader(ByteAccess.fromBytes(toBytes()));
	}

	public function toCodeArray ():Array<Int> {
		var res = [];
		eachCode(function (c) res.push(c));
		return res;
	}

	@:op(A > B) inline function opGreaterThan (other:Ucs2) {
		return this > other.impl();
	}

	@:op(A < B) inline function opLessThan (other:Ucs2) {
		return this < other.impl();
	}

	@:op(A <= B) inline function opLessThanOrEq (other:Ucs2) {
		return this <= other.impl();
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Ucs2) {
		return this >= other.impl();
	}


}
#else

import haxe.i18n.ByteAccess;

typedef Ucs2Impl = ByteAccess;

@:allow(haxe.i18n)
abstract Ucs2(ByteAccess) {

	public var length(get,never) : Int;

	public inline function new(str:String)  {
		this = haxe.i18n.Ucs2Tools.nativeStringToImpl(str); 
	}

	public function getReader ():Ucs2Reader {
		return new Ucs2Reader(this);
	}

	inline function get_length():Int {
		return Ucs2Tools.strLength(this);
	}

	public static function asByteAccess( s:Ucs2 ) : ByteAccess {
		return cast s;
	}

	inline function eachCode ( f : Int -> Void) {
		Ucs2Tools.eachCode(this, f);
	}

	public function toCodeArray () {
		var res = [];
		eachCode(function (c) res.push(c));
		return res;
	}

	public function toUpperCase() : Ucs2 {
		return fromImpl(Ucs2Tools.toUpperCase(this));
	}

	public function toLowerCase() : Ucs2 {
		return fromImpl(Ucs2Tools.toLowerCase(this));
	}

	static var empty = new Ucs2(""); 

	public function charAt(index : Int) : Ucs2 {
		return fromImpl(Ucs2Tools.charAt(this, index));
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return Ucs2Tools.charCodeAt(this, index);
	}

	public inline function indexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return Ucs2Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Ucs2, ?startIndex : Int ) : Int {
		return Ucs2Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public function split( delimiter : Ucs2 ) : Array<Ucs2> {
		return Ucs2Tools.split(this, delimiter.impl());
	}

	public function substr( pos : Int, ?len : Int ) : Ucs2 {
		return fromImpl(Ucs2Tools.substr(this, pos, len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Ucs2 {
		return fromImpl(Ucs2Tools.substring(this, startIndex, endIndex));
	}

	// private helpers
	static inline function fromByteAccess (bytes:ByteAccess):Ucs2 {
		return cast bytes;
	}
	static inline function fromImpl (impl:ByteAccess):Ucs2 {
		return cast impl;
	}
	inline function impl ():ByteAccess {
		return this;
	}

	public function toNativeString() : String {
		return Ucs2Tools.toNativeString(this);
	}

	public static inline function fromCharCode( code : Int ) : Ucs2 {
		return fromImpl(Encoding.charCodeToUcs2ByteAccess(code));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Ucs2 {
		return fromImpl(ByteAccess.fromBytes(bytes).copy());
	}

	public function toUtf8() : Utf8 {
		return Utf8.fromByteAccess(Encoding.convertUcs2toUtf8(getReader(), StrictConversion));
	}

	public function toUtf16() : Utf16 {
		return Utf16.fromBytes(toBytes());
	}

	@:op(A == B) inline function opEq (other:Ucs2) {
		return this.equal(asByteAccess(other));
	}

	@:op(A != B) inline function opNotEq (other:Ucs2) {
		return !opEq(other);
	}

	@:op(A + B) inline function opAdd (other:Ucs2) {
		return fromImpl(this.append(asByteAccess(other)));
	}


	function compare (other:Ucs2):Int {
		return Ucs2Tools.compare(this, other.impl());
	}

	@:op(A > B) inline function opGreaterThan (other:Ucs2) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Ucs2) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Ucs2) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Ucs2) {
		return compare(other) >= 0;
	}

	
}

#end