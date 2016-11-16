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

/*
#if (lua)

@:allow(haxe.i18n)
abstract Utf8(String) {

	public var length(get,never) : Int;

	public function new(str:String) : Void {
		this = str;
	}

	function get_length():Int {
		return this.length;
	}

	public function toUpperCase() : Utf8 {
		return new Utf8(this.toUpperCase());
	}

	public function toLowerCase() : Utf8 {
		return new Utf8(this.toLowerCase());
	}

	public function charAt(index : Int) : Utf8 {
		return new Utf8(this.charAt(index));
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	public function indexOf( str : Utf8, ?startIndex : Int ) : Int {
		return this.indexOf(str.toNativeString(),startIndex);
	}

	public function lastIndexOf( str : Utf8, ?startIndex : Int ) : Int {
		if (startIndex == null) { // required for flash
			return this.lastIndexOf(str.toNativeString());
		}
		return this.lastIndexOf(str.toNativeString(), startIndex);
		
	}

	public function split( delimiter : Utf8 ) : Array<Utf8> {
		return cast this.split(delimiter.toNativeString());
	}

	public function substr( pos : Int, ?len : Int ) : Utf8 {
		if (len == null) return new Utf8(this.substr(pos)); // required for flash
		return new Utf8(this.substr(pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Utf8 {
		if (endIndex == null) return new Utf8(this.substring(startIndex));
		return new Utf8(this.substring(startIndex,endIndex));
	}

	public function toNativeString() : String {
		return this;
	}

	public static inline function fromCharCode( code : Int ) : Utf8 {
		return new Utf8(String.fromCharCode(code));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		return throw "not implemented";
	}

	public static inline function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {

		var i = 0;
		var res = "";
		var bytes = ByteAccess.fromBytes(bytes);
		while (i < bytes.length) {
			var code = (bytes.fastGet(i) << 8) | bytes.fastGet(i+1);
			res += String.fromCharCode(code);
			i+=2;
		}
		return new Utf8(res);
	}


	@:op(A == B) inline function opEq (other:Utf8) {
		return this == other.toNativeString();
	}

	@:op(A + B) inline function opAdd (other:Utf8) {
		return fromNativeString(this + other.toNativeString());
	}

	@:op(A != B) inline function opNotEq (other:Utf8) {
		return !opEq(other);
	}

	public function toUcs2() : Ucs2 {
		return EncodingTools.utf8ToUcs2(new Utf8(this));
	}

	public static inline function fromByteAccess (bytes:ByteAccess):Utf8 {
		return throw "not implemented";
	}
	

	public function toUtf16() : Utf16 {
		return EncodingTools.utf8ToUtf16(new Utf8(this));
	}

	public static inline function fromNativeString (str:String):Utf8 {
		return new Utf8(str);
	}

	public function toCodeArray ():Array<Int> {
		var res = [];
		for (i in 0...length) {
			res.push(charCodeAt(i));
		}
		return res;
	}

	public static inline function asByteAccess (s:Utf8):ByteAccess {
		return ByteAccess.fromBytes(s.toBytes());
	}

	@:op(A > B) inline function opGreaterThan (other:Utf8) {
		return this > other.toNativeString();
	}
	@:op(A < B) inline function opLessThan (other:Utf8) {
		return this < other.toNativeString();
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf8) {
		return this <= other.toNativeString();
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf8) {
		return this >= other.toNativeString();
	}


}
#else
*/
import haxe.io.Bytes;
import haxe.i18n.Utf8Tools as Helper;

/**
	Cross platform Utf8 string API.
**/

/*
typedef Utf8ImplData = {
	bytes : ByteAccess,
	stringLength : Int
}

// impl has o(1) access to utf8 length
abstract Utf8Impl(Utf8ImplData) {
	inline function new (ba:ByteAccess, stringLength:Int) {
		this = {
			bytes : ba,
			stringLength : stringLength
		}
	}

	inline function fastGet (i) {
		return this.bytes.fastGet(i);
	}

	inline function byteLength (i) {
		return this.bytes.length;
	}

	public function toBytes() : haxe.io.Bytes {
		return this.bytes.copy().toBytes();
	}

	public function append(other:Utf8Impl) : Utf8Impl {
		var otherImpl = other.impl();
		var newBytes = this.bytes.append(otherImpl.bytes);
		return new Utf8Impl(newBytes, this.stringLength + otherImpl.stringLength);
	}

	inline function sub (i, size, newStringLength) {
		return new Utf8Impl(this.bytes.sub(i,size), newStringLength);
	}

	inline function impl () {
		return this;
	}
}
*/


@:allow(haxe.i18n)
abstract Utf8(ByteAccess) {

	public var length(get,never) : Int;
	
	public function new(str:String) : Void {
		this = asByteAccess(Utf8.fromNativeString(str));
	}
	
	public function toUpperCase() : Utf8 {
		var ba = Helper.toUpperCase(this, length);
		return fromByteAccessAndSize(ba, length);
	}
	
	public function toLowerCase() : Utf8 {
		var ba = Helper.toLowerCase(this, length);
		return fromByteAccessAndSize(ba, length);
	}
	
	public function charAt(index : Int) : Utf8 {
		var ba = Helper.charAt(this, index);
		return fromByteAccessAndSize(ba, 1);
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return Helper.charCodeAt(this, index);
	}
	
	public function indexOf( str : Utf8, ?startIndex : Int ) : Int
	{
		 return Helper.indexOf(this, asByteAccess(str), str.length, startIndex);
	}

	public function lastIndexOf( str : Utf8, ?startIndex : Int ) : Int {
		return Helper.lastIndexOf(this, asByteAccess(str), str.length, startIndex);
	}

	public function split( delimiter : Utf8 ) : Array<Utf8>
	{
		return Helper.split(this, asByteAccess(delimiter), delimiter.length, function (b,s) return fromByteAccessAndSize(b,s));
	}
	
	@:analyzer(no_code_motion) public function substr( pos : Int, ?len : Int ) : Utf8 {

		return Helper.substr(this, pos, function (b,s) return fromByteAccessAndSize(b,s), len);
	}
	
	public function substring( startIndex : Int, ?endIndex : Int ) : Utf8 {
		return Helper.substring(this, length, startIndex, function (b,s) return fromByteAccessAndSize(b,s), endIndex);
	}
	
	public static function fromCharCode( code : Int ) : Utf8
	{
		var ba = Helper.fromCharCode(code);
		return fromByteAccessAndSize(ba, 1);
	}

	@:op(A + B) inline function opAdd (other:Utf8) {
		return fromByteAccess(this.append(asByteAccess(other)));
	}

	@:op(A == B) public function opEq (other:Utf8) {
		return this.equal(asByteAccess(other));
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

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {
		return fromByteAccess(ByteAccess.fromBytes(bytes).copy());
	}

	public function toNativeString() : String {
		return this.getString(0, this.length);
	}
 	
	public function toUcs2() : Ucs2 {
		return EncodingTools.utf8ToUcs2(fromByteAccess(this));
	}
	
 	public function toUtf16 ():Utf16 {
		return EncodingTools.utf8ToUtf16(fromByteAccess(this));
	}
	
	public function toBytes() : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	public function toCodeArray ():Array<Int> {
		return Helper.toCodeArray(this);
	}

	// private api

	static inline function fromByteAccessAndSize (ba:ByteAccess, size:Int) {
		return fromByteAccess(ba);
	}

	function get_length() {
		 return Helper.getLength(this);
	}

	static function empty () {
		return new Utf8("");
	}
	
	static function asByteAccess (s:Utf8):ByteAccess {
		return cast s;
	}
	
	static inline function fromByteAccess (bytes:ByteAccess):Utf8 {
		return cast bytes;
	}
	
	static function fromNativeString (s:String):Utf8 {
		return fromByteAccess(Helper.nativeStringToByteAccess(s));
 	}

	function compare (other:Utf8):Int {
		return Helper.compare(this, length, asByteAccess(other), other.length);
	}
}

//#end