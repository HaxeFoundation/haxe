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

import haxe.io.Bytes;
import haxe.i18n.Utf8Tools;


typedef Utf8Impl = {
	b : ByteAccess,
	length : Int
}

abstract Utf8(Utf8Impl) {

	public var length(get,never) : Int;
	
	public inline function new(str:String) : Void {
		this = Utf8Tools.nativeStringToImpl(str);
	}
	
	public inline function toUpperCase() : Utf8 {
		return fromImpl(Utf8Tools.toUpperCase(this));
	}
	
	public inline function toLowerCase() : Utf8 {
		return fromImpl(Utf8Tools.toLowerCase(this));
	}
	
	public inline function charAt(index : Int) : Utf8 {
		return fromImpl(Utf8Tools.charAt(this, index));
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return Utf8Tools.charCodeAt(this, index);
	}
	
	public inline function indexOf( str : Utf8, ?startIndex : Int ) : Int {
		 return Utf8Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Utf8, ?startIndex : Int ) : Int {
		return Utf8Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public function split( delimiter : Utf8 ) : Array<Utf8> {
		return Utf8Tools.split(this, delimiter.impl());
	}
	
	public inline function substr( pos : Int, ?len : Int ) : Utf8 {
		return fromImpl(Utf8Tools.substr(this, pos, len));
	}
	
	public inline function substring( startIndex : Int, ?endIndex : Int ) : Utf8 {
		return fromImpl(Utf8Tools.substring(this, startIndex, endIndex));
	}
	
	public static inline function fromCharCode( code : Int ) : Utf8 {
		return fromImpl(Utf8Tools.fromCharCode(code));
	}

	@:op(A + B) inline function opAdd (other:Utf8) {
		return fromImpl(Utf8Tools.append(this, other.impl()));
	}

	@:op(A == B) inline function opEq (other:Utf8) {
		return Utf8Tools.equal(this, other.impl());
	}

	@:op(A != B) inline function opNotEq (other:Utf8) {
		return !opEq(other);
	}
	
	@:op(A > B) inline function opGreaterThan (other:Utf8) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Utf8) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf8) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf8) {
		return compare(other) >= 0;
	}

	// additional public api

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public inline function toNativeString() : String {
		return Utf8Tools.toNativeString(this);
	}
 	
	public inline function toUcs2() : Ucs2 {
		return EncodingUtf8Tools.utf8ToUcs2(fromImpl(this));
	}
	
 	public inline function toUtf16 ():Utf16 {
		return EncodingUtf8Tools.utf8ToUtf16(fromImpl(this));
	}
	
	public inline function toBytes() : haxe.io.Bytes {
		return Utf8Tools.toBytes(this);
	}

	public inline function toCodeArray ():Array<Int> {
		return Utf8Tools.toCodeArray(this);
	}

	public static inline function fromByteAccess (ba:ByteAccess) {
		return fromImpl(Utf8Tools.fromByteAccess(ba));
	}

	public inline function getByteReader ():ByteReader {
		return Utf8Tools.getByteReader(this);
	}

	// private api

	inline function get_length() {
		return Utf8Tools.strLength(this);
	}

	inline function compare (other:Utf8):Int {
		return Utf8Tools.compare(this, other.impl());
	}

	// wrap and unwrap

	inline function impl ():Utf8Impl {
		return this;
	}
	
	static inline function fromImpl (impl:Utf8Impl):Utf8 {
		return cast impl;
	}
}
