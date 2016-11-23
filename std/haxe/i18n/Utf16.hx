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

@:structInit class Utf16Impl {
	public var b(default, null) : ByteAccess;
	public var length(default,null) : Int;
}

@:allow(haxe.i18n)
@:access(haxe.i18n.Utf16Tools)
abstract Utf16(Utf16Impl) {

	public var length(get,never) : Int;

	public function new(str:String) : Void {
		this = Utf16Tools.nativeStringToImpl(str);
	}

	public inline function toUpperCase() : Utf16 {
		return fromImpl(Utf16Tools.toUpperCase(this));
	}
	public inline function toLowerCase() : Utf16 {
		return fromImpl(Utf16Tools.toLowerCase(this));
	}

	public inline function charAt(index : Int) : Utf16 {
		return fromImpl(Utf16Tools.charAt(this, index));
	}

	public inline function charCodeAt( index : Int) : Null<Int> {
		return Utf16Tools.charCodeAt(this, index);
	}

	public inline function indexOf( str : Utf16, ?startIndex : Int ) : Int {
		return Utf16Tools.indexOf(this, str.impl(), startIndex);
	}

	public inline function lastIndexOf( str : Utf16, ?startIndex : Int ) : Int {
		return Utf16Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public inline function split( delimiter : Utf16 ) : Array<Utf16> {
		return Utf16Tools.split(this, delimiter.impl());
	}

	public inline function substr( pos : Int, ?len : Int ) : Utf16 {
		return fromImpl(Utf16Tools.substr(this, pos, len));
	}

	public inline function substring( startIndex : Int, ?endIndex : Int ) : Utf16 {
		return fromImpl(Utf16Tools.substring(this, startIndex, endIndex));
	}

	public static inline function fromCharCode( code : Int ) : Utf16 {
		return fromImpl(Utf16Tools.fromCharCode(code));
	}

	@:op(A + B) inline function opAdd (other:Utf16):Utf16 {
		return fromImpl(Utf16Tools.append(this, other.impl()));
	}

	@:op(A == B) inline function opEq (other:Utf16) {
		return Utf16Tools.equal(this, other.impl());
	}
	
	@:op(A != B) inline function opNotEq (other:Utf16) {
		return !opEq(other);
	}

	@:op(A > B) inline function opGreaterThan (other:Utf16) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Utf16) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf16) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf16) {
		return compare(other) >= 0;
	}

	// additional public api

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf16 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public inline function toNativeString() : String {
		return Utf16Tools.toNativeString(this);//this.getString(0, this.length);
	}

	public inline function toUcs2() : Ucs2 {
		return Ucs2.fromByteAccess(this.b);
	}

	public inline function toUtf8 ():Utf8 {
		return Utf8.fromByteAccess(Encoding.convertUtf16toUtf8(getReader(), StrictConversion));
	}

	public inline function toBytes() : haxe.io.Bytes {
		return Utf16Tools.toBytes(this);
	}

	public inline function toCodeArray () {
		return Utf16Tools.toCodeArray(this);
	}
	
	public static inline function fromByteAccess (ba:ByteAccess) {
		return fromImpl(Utf16Tools.fromByteAccess(ba));
	}

	public inline function getReader ():Utf16Reader
	{
		return new Utf16Reader(this.b);
	}

	// private helpers

	inline function get_length() {
		return Utf16Tools.strLength(this);
	}
	
	static inline function fromImpl (impl:Utf16Impl):Utf16 {
		return cast impl;
	}

	inline function impl ():Utf16Impl {
		return this;
	}

	inline function compare (other:Utf16):Int {
		return Utf16Tools.compare(this, other.impl());
	}
	
}
