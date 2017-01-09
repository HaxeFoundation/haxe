/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package haxe.io;

@:coreApi
class Bytes {

	public var length(default,null) : Int;
	var b : hl.Bytes;

	function new(b:hl.Bytes,length:Int) : Void {
		this.b = b;
		this.length = length;
	}

	inline function out(pos:Int) : Bool {
		return (pos:UInt) >= (length : UInt);
	}

	inline function outRange(pos:Int,len:Int) : Bool {
		return pos < 0 || len < 0 || ((pos+len):UInt) > (length : UInt);
	}

	public function get( pos : Int ) : Int {
		return if( out(pos) ) 0 else b[pos];
	}

	public function set( pos : Int, v : Int ) : Void {
		if( out(pos) ) throw Error.OutsideBounds;
		b[pos] = v;
	}

	public function blit( pos : Int, src : Bytes, srcpos : Int, len : Int ) : Void {
		if( outRange(pos, len) || src.outRange(srcpos,len) ) throw Error.OutsideBounds;
		b.blit(pos, src.b, srcpos, len);
	}

	public function fill( pos : Int, len : Int, value : Int ) : Void {
		if( outRange(pos,len) ) throw Error.OutsideBounds;
		b.fill(pos, len, value);
	}

	public function sub( pos : Int, len : Int ) : Bytes {
		if( outRange(pos,len) ) throw Error.OutsideBounds;
		return new Bytes(b.sub(pos, len), len);
	}

	public function compare( other : Bytes ) : Int {
		var len = length < other.length ? length : other.length;
		var r = b.compare(0, other.b, 0, len);
		if( r == 0 )
			r = length - other.length;
		return r;
	}

	public function getDouble( pos : Int ) : Float {
		return if( out(pos + 7) ) 0. else b.getF64(pos);
	}

	public function getFloat( pos : Int ) : Float {
		return if( out(pos + 3) ) 0. else b.getF32(pos);
	}

	public function setDouble( pos : Int, v : Float ) : Void {
		if( out(pos + 7) ) throw Error.OutsideBounds;
		b.setF64(pos, v);
	}

	public function setFloat( pos : Int, v : Float ) : Void {
		if( out(pos + 3) ) throw Error.OutsideBounds;
		b.setF32(pos, v);
	}

	public inline function getUInt16( pos : Int ) : Int {
		return if( out(pos + 1) ) 0 else b.getUI16(pos);
	}

	public inline function setUInt16( pos : Int, v : Int ) : Void {
		if( out(pos + 1) ) throw Error.OutsideBounds;
		b.setUI16(pos, v);
	}

	public function getInt32( pos : Int ) : Int {
		return if( out(pos + 3) ) 0 else b.getI32(pos);
	}

	public function getInt64( pos : Int ) : haxe.Int64 {
		if( out(pos + 7) )
			return haxe.Int64.ofInt(0);
		return haxe.Int64.make(b.getI32(pos+4), b.getI32(pos));
	}

	public function setInt32( pos : Int, v : Int ) : Void {
		if( out(pos + 3) ) throw Error.OutsideBounds;
		b.setI32(pos, v);
	}

	public inline function setInt64( pos : Int, v : haxe.Int64 ) : Void {
		setInt32(pos + 4, v.high);
		setInt32(pos, v.low);
	}

	public function getString( pos : Int, len : Int ) : String {
		if( outRange(pos,len) ) throw Error.OutsideBounds;

		var b = new hl.Bytes(len + 1);
		b.blit(0, this.b, pos, len);
		b[len] = 0;
		return @:privateAccess String.fromUTF8(b);
	}

	@:deprecated("readString is deprecated, use getString instead")
	@:noCompletion
	public inline function readString(pos:Int, len:Int):String {
		return getString(pos, len);
	}

	public function toString() : String {
		return getString(0,length);
	}

	public function toHex() : String {
		var s = new StringBuf();
		var chars = [];
		var str = "0123456789abcdef";
		for( i in 0...str.length )
			chars.push(str.charCodeAt(i));
		for( i in 0...length ) {
			var c = get(i);
			s.addChar(chars[c >> 4]);
			s.addChar(chars[c & 15]);
		}
		return s.toString();
	}

	public inline function getData() : BytesData {
		return new haxe.io.BytesData(b,length);
	}

	public static function alloc( length : Int ) : Bytes {
		var b = new hl.Bytes(length);
		b.fill(0, length, 0);
		return new Bytes(b,length);
	}

	public static function ofString( s : String ) : Bytes @:privateAccess {
		var size = 0;
		var b = s.bytes.utf16ToUtf8(0, size);
		return new Bytes(b,size);
	}

	public static function ofData( b : BytesData ) : Bytes {
		return new Bytes(b.bytes,b.length);
	}

	public inline static function fastGet( b : BytesData, pos : Int ) : Int {
		return b[pos];
	}

}
