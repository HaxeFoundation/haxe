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
	Cross platform Utf16 string API.
**/

#if false
import haxe.io.Bytes;
import haxe.io.BytesData;
abstract Utf16(String) {

	/**/
	public var length(get,never) : Int;

	public function new(str:String) : Void {
		this = str;
	}

		function get_length() {
		return this.length;
	}

	public function toUpperCase() : Utf16 {
		return new Utf16(this.toUpperCase());
	}

	public function toLowerCase() : Utf16 {
		return new Utf16(this.toLowerCase());
	}

	public function isValidUcs2 ():Bool {
		for (i in 0...this.length) {
			var code = charCodeAt(i);
			if (EncodingTools.getUtf16CodeSize(code) == 4) return false;
		}
		return true;
	}

	public function charAt(index : Int) : Utf16 {
		return new Utf16(this.charAt(index));
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return this.charCodeAt(index);
	}

	public inline function fastCodeAt( index : Int) : Int {
		return StringTools.fastCodeAt(this, index);
	}

	public function indexOf( str : Utf16, ?startIndex : Int ) : Int {
		return this.indexOf(str.toNativeString(),startIndex);
	}

	public function lastIndexOf( str : Utf16, ?startIndex : Int ) : Int {
		return this.lastIndexOf(str.toNativeString(),startIndex);
	}

	public function split( delimiter : Utf16 ) : Array<Utf16> {
		return cast this.split(delimiter.toNativeString());
	}

	public function substr( pos : Int, ?len : Int ) : Utf16 {
		return new Utf16(this.substr(pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Utf16 {
		return new Utf16(this.substring(startIndex,endIndex));
	}

	@:op(A == B) inline function opEq (other:Utf16) {
		return this == other.toNativeString();
	}

	@:op(A + B) inline function opAdd (other:Utf16) {
		return fromNativeString(this + other.toNativeString());
	}

	@:op(A != B) inline function opNotEq (other:Utf16) {
		return !opEq(other);
	}

	public static function fromCharCode( code : Int ) : Utf16 {

		return fromByteAccess(EncodingTools.charCodeToUtf16ByteAccess(code));
	}

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf16 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public static function fromNativeString (s:String):Utf16 {
		return new Utf16(s);
	}

	public function toNativeString() : String {
		return this;
	}

	public static inline function asByteAccess (s:Utf16):ByteAccess {
		return ByteAccess.fromBytes(s.toBytes());
	}

	public static inline function fromByteAccess (bytes:ByteAccess):Utf16 {
		#if cs
		return new Utf16(cs.system.text.Encoding.BigEndianUnicode.GetString(bytes.asBytesData(), 0, bytes.asBytesData().length));
		#elseif java
		try
			return new Utf16(new String(bytes.asBytesData(), 0, bytes.asBytesData().length, "UTF-16BE"))
		catch (e:Dynamic) throw e;
		#end

	}

	public function toUcs2() : Ucs2 {
		return EncodingTools.utf16ToUcs2(new Utf16(this));
	}

	inline function eachCode ( f : Int -> Void) {
		for (i in 0...length) {
			var code = fastCodeAt(i);
			f(code);
		}
	}

	public function toCodeArray () {

		var res = [];
		eachCode(function (c) res.push(c));
		return res;
	}

	public function toUtf8 ():Utf8 {
		return EncodingTools.utf16ToUtf8(new Utf16(this));
	}

	public function toBytes(  ) : haxe.io.Bytes {
		#if cs
		var b = cs.system.text.Encoding.BigEndianUnicode.GetBytes(this);
		return Bytes.ofData(b);
		#elseif java
		try
		{
			var b:haxe.io.BytesData = untyped this.getBytes("UTF-16BE");
			return Bytes.ofData(b);
		}
		catch (e:Dynamic) throw e;
		#end
	}

	@:op(A > B) inline function opGreaterThan (other:Utf16) {
		return this > other.toNativeString();
	}
	@:op(A < B) inline function opLessThan (other:Utf16) {
		return this < other.toNativeString();
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf16) {
		return this <= other.toNativeString();
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf16) {
		return this >= other.toNativeString();
	}
}

#else
// bytes based implementation

typedef Utf16Impl = {
	b : ByteAccess,
	length : Int
}

@:allow(haxe.i18n)
@:access(haxe.i18n.Utf16Tools)
abstract Utf16(Utf16Impl) {

	public var length(get,never) : Int;

	public function new(str:String) : Void {
		this = Utf16Tools.nativeStringToImpl(str);
	}

	public function toUpperCase() : Utf16 {
		return fromImpl(Utf16Tools.toUpperCase(this));
	}
	public function toLowerCase() : Utf16 {
		return fromImpl(Utf16Tools.toLowerCase(this));
	}

	public function charAt(index : Int) : Utf16 {
		return fromImpl(Utf16Tools.charAt(this, index));
	}

	public function charCodeAt( index : Int) : Null<Int> {
		return Utf16Tools.charCodeAt(this, index);
	}

	public function indexOf( str : Utf16, ?startIndex : Int ) : Int {
		return Utf16Tools.indexOf(this, str.impl(), startIndex);
	}

	public function lastIndexOf( str : Utf16, ?startIndex : Int ) : Int {
		return Utf16Tools.lastIndexOf(this, str.impl(), startIndex);
	}

	public function split( delimiter : Utf16 ) : Array<Utf16> {
		return Utf16Tools.split(this, delimiter.impl());
	}

	public function substr( pos : Int, ?len : Int ) : Utf16 {
		return fromImpl(Utf16Tools.substr(this, pos, len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : Utf16 {
		return fromImpl(Utf16Tools.substring(this, startIndex, endIndex));
	}

	public static function fromCharCode( code : Int ) : Utf16 {
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

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf16 {
		return fromByteAccess(ByteAccess.fromBytes(bytes));
	}

	public function toNativeString() : String {
		return Utf16Tools.toNativeString(this);//this.getString(0, this.length);
	}

	public function toUcs2() : Ucs2 {
		return EncodingTools.utf16ToUcs2(fromImpl(this));
	}

	public function toUtf8 ():Utf8 {
		return EncodingTools.utf16ToUtf8(fromImpl(this));
	}

	public function toBytes() : haxe.io.Bytes {
		return Utf16Tools.toBytes(this);
	}

	public function toCodeArray () {
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

	function get_length() {
		return Utf16Tools.strLength(this);
	}

	static inline function getCharSize (start2Bytes:Int):Int {
		return if (EncodingTools.isHighSurrogate(start2Bytes)) 4 else 2;
	}

	

	static function fromImpl (impl:Utf16Impl):Utf16 {
		return cast impl;
	}

	function impl ():Utf16Impl {
		return this;
	}

	

	inline function compare (other:Utf16):Int {
		return Utf16Tools.compare(this, other.impl());
	}

	
}

#end