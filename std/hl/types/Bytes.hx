/*
 * Copyright (C)2005-2016 Haxe Foundation
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
package hl.types;

@:coreType abstract Bytes {

	@:extern public inline function new( v : Int ) {
		this = alloc(v);
	}

	@:hlNative("std","bytes_blit") public function blit( pos : Int, src : Bytes, srcPos : Int, len : Int ) : Void {
	}

	@:extern @:arrayAccess public inline function getUI8( pos : Int ) : Int {
		return untyped $bgetui8(this,pos);
	}

	@:extern @:arrayAccess public inline function setUI8( pos : Int, value : Int ) : Int {
		untyped $bsetui8(this,pos,value);
		return value;
	}

	@:extern public inline function getI32( pos : Int ) : Int {
		return untyped $bgeti32(this,pos);
	}

	public inline function getUI16( pos : Int ) : Int {
		return untyped $bgetui16(this, pos);
	}

	public inline function setUI16( pos : Int, v : Int ) {
		untyped $bsetui16(this,pos,v);
	}

	@:extern public inline function getF32( pos : Int ) : F32 {
		return untyped $bgetf32(this,pos);
	}

	@:extern public inline function getF64( pos : Int ) : Float {
		return untyped $bgetf64(this,pos);
	}

	@:extern public inline function setI32( pos : Int, value : Int ) : Void {
		untyped $bseti32(this, pos, value);
	}

	@:extern public inline function setF32( pos : Int, value : F32 ) : Void {
		untyped $bsetf32(this, pos, value);
	}

	@:extern public inline function setF64( pos : Int, value : Float ) : Void {
		untyped $bsetf64(this, pos, value);
	}

	@:hlNative("std","alloc_bytes")
	static function alloc( size : Int ) : Bytes {
		return null;
	}
	
	@:hlNative("std","parse_int")
	public function parseInt( pos : Int, size : Int ) : Null<Int> {
		return null;
	}

	@:hlNative("std","parse_float")
	public function parseFloat( pos : Int, size : Int ) : Float {
		return 0.;
	}

	@:hlNative("std","bytes_compare")
	public function compare( pos : Int, bytes : Bytes, bytesPos : Int, size : Int ) : Int {
		return 0;
	}

	@:hlNative("std","bytes_find")
	public function find( pos : Int, size : Int, bytes : Bytes, bytesPos : Int, bytesSize : Int ) : Int {
		return 0;
	}

	@:hlNative("std","bytes_fill")
	public function fill( pos : Int, size : Int, v : Int ) : Void {
	}

	@:hlNative("std","bsort_i32")
	public function sortI32( pos : Int, length : Int, f : Int->Int->Int ) : Void {
	}

	@:hlNative("std","bsort_f64")
	public function sortF64( pos : Int, length : Int, f : Float->Float->Int ) : Void {
	}

	public function sub( pos : Int, size : Int ) {
		var b = new Bytes(size);
		b.blit(0, this, pos, size);
		return b;
	}

	@:hlNative("std", "ucs2length")
	public function ucs2Length( bytePos : Int ) : Int {
		return 0;
	}

	@:hlNative("std","hash")
	function hash() : Int {
		return 0;
	}

	@:hlNative("std","utf8_to_utf16")
	public function utf8ToUtf16( bytePos : Int, outSize : Ref<Int> ) : Bytes {
		return null;
	}

	@:hlNative("std","utf16_to_utf8")
	public function utf16ToUtf8( bytePos : Int, outSize : Ref<Int> ) : Bytes {
		return null;
	}

	@:hlNative("std", "ucs2_upper")
	function ucs2Upper( bytePos : Int, size : Int ) : Bytes {
		return null;
	}

	@:hlNative("std", "ucs2_lower")
	function ucs2Lower( bytePos : Int, size : Int ) : Bytes {
		return null;
	}

	@:hlNative("std", "url_encode")
	function urlEncode( outSize : Ref<Int> ) : Bytes {
		return null;
	}

	@:hlNative("std", "url_decode")
	function urlDecode( outSize : Ref<Int> ) : Bytes {
		return null;
	}

	@:hlNative("std","value_to_string")
	public static function ofValue( v : Dynamic, length : Ref<Int> ) : Bytes {
		return null;
	}
}