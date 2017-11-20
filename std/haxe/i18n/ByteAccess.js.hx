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
import haxe.io.BytesData;
import haxe.io.Error;

#if !nodejs
import js.html.compat.Uint8Array;
#end

import js.html.Uint8Array;

abstract ByteAccess(Uint8Array) {

	public var length(get, never):Int;

	inline function new (length:Int) {
		this = Uint8ArrayTools.alloc(length);
	}

	/* static constructors */

	public static inline function alloc (length:Int) {
		return new ByteAccess(length);
	}

	public static inline function ofData (data:BytesData):ByteAccess {
		return fromImpl(Uint8ArrayTools.wrapData(data));
	}

	public static inline function fromBytes (b:Bytes):ByteAccess {
		return fromImpl(Uint8ArrayTools.wrapData(b.sub(0, b.length).getData()));
	}

	/* gets */

	public inline function get( pos : Int ) : Int {
		return Uint8ArrayTools.get(this, pos);
	}

	public inline function getInt32LE( pos : Int ) : Int {
		return get(pos) | (get(pos+1) << 8) | (get(pos+2) << 16) | (get(pos+3) << 24);
	}

	public inline function getInt16LE( pos : Int ) : Int {
		return get(pos) | (get(pos+1) << 8);
	}

	public inline function getString( pos : Int, len : Int ) : String {
		return Uint8ArrayTools.getString(this, pos, len);
	}

	public inline function fastGet (pos:Int):Int {
		return Uint8ArrayTools.fastGet(this, pos);
	}

	/* sets */

	public inline function set( pos : Int, v : Int ) : Void {
		Uint8ArrayTools.set(this, pos, v);
	}

	public inline function setInt16LE( pos : Int, v : Int ) : Void {
		Uint8ArrayTools.set(this, pos, v & 0xFF );
		Uint8ArrayTools.set(this, pos+1, (v >> 8) & 0xFF );
	}

	public inline function setInt32LE( pos : Int, v : Int ) : Void {
		Uint8ArrayTools.set(this, pos, v & 0xFF );
		Uint8ArrayTools.set(this, pos+1, (v >> 8) & 0xFF );
		Uint8ArrayTools.set(this, pos+2, (v >> 16) & 0xFF );
		Uint8ArrayTools.set(this, pos+3, (v >> 24) & 0xFF );
	}

	/* sets end */

	public inline function sub(pos:Int, len:Int):ByteAccess {
		return fromImpl(Uint8ArrayTools.sub(this, pos, len));
	}

	public inline function copy ():ByteAccess {
		return sub(0, length);
	}

	public inline function blit (pos : Int, src : ByteAccess, srcpos : Int, len : Int):Void {
		return Uint8ArrayTools.blit(this, pos, src.impl(), srcpos, len);
	}

	public inline function append (other : ByteAccess):ByteAccess {
		var c = Uint8ArrayTools.alloc(this.length + other.length);
		c.set(this);
		c.set(other.impl(), this.length);
		return fromImpl(c);
	}

	/* compare, equal */

	public function equal (other:ByteAccess) {
		if (this == other.impl()) return true;
		var a = fromImpl(this);
		var b = other;

		for (i in 0...a.length) {
			if (a.fastGet(i) != b.fastGet(i)) return false;
		}
		return true;
	}

	public function compare (other:ByteAccess) {
		if (this == other.impl()) return 0;
		var a = fromImpl(this);
		var b = other;


		var min = a.length < b.length ? a.length : b.length;

		for (i in 0...min) {
			var b1 = a.fastGet(i);
			var b2 = b.fastGet(i);
			if (b1 < b2) return -1;
			if (b1 > b2) return 1;
		}
		if (a.length < b.length) return -1;
		if (a.length > b.length) return 1;
		return 0;
	}

	/* conversions */

	public function toString ():String {
		var res = [];
		for (i in 0...length) {

			res.push(fastGet(i));
		}
		return res.join(",");
	}

	public inline function toBytes ():Bytes {
		return Bytes.ofData(sub(0, length).getData());
	}

	/* Internal functions */

	static inline function fromImpl (b:Uint8Array):ByteAccess {
		return cast b;
	}

	inline function impl ():Uint8Array {
		return this;
	}

	@:allow(haxe.i18n) inline function getData():BytesData {
		return Uint8ArrayTools.getData(this);
	}

	inline function get_length ():Int {
		return Uint8ArrayTools.getByteLength(this);
	}
}

@:allow(haxe.i18n)
class Uint8ArrayTools {

	static inline function get( b:Uint8Array, pos : Int ) : Int {
		return b[pos];
	}

	static inline function fastGet (b:Uint8Array, pos:Int):Int {
		return (b:Dynamic)[pos];
	}

	static function getString( b:Uint8Array, pos : Int, len : Int ) : String {
		if( pos < 0 || len < 0 || pos + len > getByteLength(b) ) throw Error.OutsideBounds;
		var s = "";

		var fcc = String.fromCharCode;
		var i = pos;
		var max = pos+len;
		// utf8-decode and utf16-encode
		while( i < max ) {
			var c = b[i++];
			if( c < 0x80 ) {
				if( c == 0 ) break;
				s += fcc(c);
			} else if( c < 0xE0 )
				s += fcc( ((c & 0x3F) << 6) | (b[i++] & 0x7F) );
			else if( c < 0xF0 ) {
				var c2 = b[i++];
				s += fcc( ((c & 0x1F) << 12) | ((c2 & 0x7F) << 6) | (b[i++] & 0x7F) );
			} else {
				var c2 = b[i++];
				var c3 = b[i++];
				var u = ((c & 0x0F) << 18) | ((c2 & 0x7F) << 12) | ((c3 & 0x7F) << 6) | (b[i++] & 0x7F);
				// surrogate pair
				s += fcc( (u >> 10) + 0xD7C0 );
				s += fcc( (u & 0x3FF) | 0xDC00 );
			}
		}
		return s;
	}

	static inline function set( b:Uint8Array, pos : Int, v : Int ) : Void {
		b[pos] = v & 0xFF;
	}

	static inline function getByteLength (b:Uint8Array):Int {
		return getData(b).byteLength;
	}

	static inline function alloc (length:Int):Uint8Array {
		var data = new BytesData(length);
		return Uint8ArrayTools.wrapData(data);
	}

	static inline function getArrayFromData(b:BytesData):Uint8Array {
		return (b:Dynamic).bytes;
	}

	static inline function getData(b:Uint8Array):BytesData {
		return (b:Dynamic).bufferValue;
	}

	static inline function wrapData (data:BytesData):Uint8Array {
		var a = new js.html.Uint8Array(data);
		(a:Dynamic).bufferValue = data; // some impl does not return the same instance in .buffer
		(data:Dynamic).bytes = a;
		return a;
	}

	static inline function sub(b:Uint8Array, pos:Int, len:Int):Uint8Array {
		if( pos < 0 || len < 0 || pos + len > getByteLength(b) ) throw Error.OutsideBounds;
		return wrapData(b.buffer.slice(pos+b.byteOffset,pos+b.byteOffset+len));
	}

	static function blit( b:Uint8Array, pos : Int, src : Uint8Array, srcpos : Int, len : Int ) : Void {
		var byteLength = getByteLength(b);
		if( pos < 0 || srcpos < 0 || len < 0 || pos + len > byteLength || srcpos + len > byteLength ) throw Error.OutsideBounds;

		var b1 = b;
		var b2 = src;
		if( b1 == b2 && pos > srcpos ) {
			var i = len;
			while( i > 0 ) {
				i--;
				b1[i + pos] = b2[i + srcpos];
			}
			return;
		}
		for( i in 0...len ) {
			b1[i + pos] = b2[i + srcpos];
		}
	}
}
