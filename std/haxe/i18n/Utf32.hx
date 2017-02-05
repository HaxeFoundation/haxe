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



import haxe.i18n.ByteAccess;

typedef Utf32Impl = ByteAccess;

@:allow(haxe.i18n)
abstract Utf32(Utf32Impl) {

	public var length(get,never) : Int;

	public inline function new(str:String)  {
		this = NativeStringTools.toUtf32(str); 
	}

	public inline function getReader ():Utf32Reader {
		return new Utf32Reader(this);
	}

	inline function get_length():Int {
		return Utf32Tools.strLength(this);
	}

	public static function asByteAccess( s:Utf32 ) : ByteAccess {
		return cast s;
	}

	inline function eachCode ( f : Int -> Void) {
		Utf32Tools.eachCode(this, f);
	}

	public function toCodeArray () {
		var res = [];
		eachCode(function (c) res.push(c));
		return res;
	}

	public function toUpperCase() : Utf32 {
		return fromImpl(Utf32Tools.toUpperCase(this));
	}

	public function toLowerCase() : Utf32 {
		return fromImpl(Utf32Tools.toLowerCase(this));
	}

	public function charAt(index : Int) : Utf32 {
		return fromImpl(Utf32Tools.charAt(this, index));
	}

	public function isValid ():Bool {
		return Utf32Tools.isValid(this);
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return Utf32Tools.charCodeAt(this, index);
	}

	public inline function indexOf( str : Utf32, ?startIndex : Int ) : Int {
		return Utf32Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Utf32, ?startIndex : Int ) : Int {
		return Utf32Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public function split( delimiter : Utf32 ) : Array<Utf32> {
		return Utf32Tools.split(this, delimiter.impl());
	}

	public function substr( pos : Int, ?len : Int ) : Utf32 {
		return fromImpl(Utf32Tools.substr(this, pos, len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Utf32 {
		return fromImpl(Utf32Tools.substring(this, startIndex, endIndex));
	}

	// private helpers
	static inline function fromByteAccess (bytes:ByteAccess):Utf32 {
		return cast bytes;
	}
	static inline function fromImpl (impl:ByteAccess):Utf32 {
		return cast impl;
	}
	inline function impl ():ByteAccess {
		return this;
	}

	//public function toNativeString() : String {
	//	return Utf32Tools.toNativeString(this);
	//}

	public static inline function fromCharCode( code : Int ) : Utf32 {
		return fromImpl(Encoding.charCodeToUtf32ByteAccess(code));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf32 {
		return fromImpl(ByteAccess.fromBytes(bytes).copy());
	}

	
	public function toUtf8() : Utf8 {
		return Utf8.fromByteAccess(Encoding.convertUtf32toUtf8(getReader(), StrictConversion));
	}
	
	public function toUtf16() : Utf16 {
		return Utf16.fromByteAccess(Encoding.convertUtf32toUtf16(getReader(), StrictConversion));
	}

	public function toUcs2() : Ucs2 {
		return toUtf16().toUcs2();
	}

	@:op(A == B) inline function opEq (other:Utf32) {
		return this.equal(asByteAccess(other));
	}

	@:op(A != B) inline function opNotEq (other:Utf32) {
		return !opEq(other);
	}

	@:op(A + B) inline function opAdd (other:Utf32) {
		return fromImpl(this.append(asByteAccess(other)));
	}


	function compare (other:Utf32):Int {
		return Utf32Tools.compare(this, other.impl());
	}

	@:op(A > B) inline function opGreaterThan (other:Utf32) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Utf32) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf32) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf32) {
		return compare(other) >= 0;
	}

	
}

