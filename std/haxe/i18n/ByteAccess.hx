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

#if cpp
using cpp.NativeArray;
#end

abstract ByteAccess(BytesData) {

	public var length(get, never):Int;

	inline function new (length:Int) {
		this = BytesDataTools.alloc(length);
	}

	/* constructors */

	public static inline function alloc (length:Int) {
		return new ByteAccess(length);
	}

	public static inline function ofData (data:BytesData):ByteAccess {
		return fromImpl(data);
	}

	public static inline function fromBytes (b:Bytes):ByteAccess {
		return fromImpl(b.getData());
	}

	/* gets */

	public inline function get( pos : Int ) : Int {
		return BytesDataTools.get(this, pos);
	}

	public inline function fastGet (pos:Int) {
		return BytesDataTools.fastGet(this, pos);
	}

	public inline function getInt32LE( pos : Int ) : Int {
		return get(pos) | (get(pos+1) << 8) | (get(pos+2) << 16) | (get(pos+3) << 24);
	}

	public inline function getInt16LE( pos : Int ) : Int {
		var lower =  get(pos);
		var upper = get(pos+1) << 8;
		return upper | lower;
	}

	public inline function getString( pos : Int, len : Int ) : String {
		return BytesDataTools.getString(this, pos, len);
	}

	/* sets */

	public inline function set( pos : Int, v : Int ) : Void {
		BytesDataTools.set(this, pos, v);
	}

	public inline function setInt16LE( pos : Int, v : Int ) : Void {
		BytesDataTools.set(this, pos, v & 0xFF );
		BytesDataTools.set(this, pos+1, (v >> 8) & 0xFF );
	}

	public inline function setInt32LE( pos : Int, v : Int ) : Void {
		BytesDataTools.set(this, pos, v & 0xFF );
		BytesDataTools.set(this, pos+1, (v >> 8) & 0xFF );
		BytesDataTools.set(this, pos+2, (v >> 16) & 0xFF );
		BytesDataTools.set(this, pos+3, (v >> 24) & 0xFF );
	}

	/* sets end */

	public inline function sub(i:Int, size:Int):ByteAccess {
		return fromImpl(BytesDataTools.sub(this, i, size));
	}

	public inline function copy ():ByteAccess {
		return fromImpl(BytesDataTools.sub(this, 0, length));
	}

	public inline function blit (pos : Int, src : ByteAccess, srcpos : Int, len : Int):Void {
		return BytesDataTools.blit(this, pos, src.impl(), srcpos, len);
	}

	public inline function append (other : ByteAccess):ByteAccess {
		var ba = alloc(length + other.length);
		ba.blit(0, fromImpl(this), 0, length);
		ba.blit(length, other, 0, other.length);
		return ba;
	}

	/* compare, equal */

	public function equal (other:ByteAccess) {
		if (this == other.impl()) return true;

		var a = fromImpl(this);
		var b = other;

		if (a.length != b.length) return false;

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
		var a = fromImpl(this);
		var res = [];
		for (i in 0...a.length) {

			res.push(a.fastGet(i));
		}
		return res.join(",");
	}

	public inline function toBytes ():Bytes {
		return Bytes.ofData(this);
	}

	@:allow(haxe.i18n) inline function getData ():BytesData {
		return impl();
	}

	/* internal helpers */

	static inline function fromImpl (b:BytesData):ByteAccess {
		return cast b;
	}

	inline function impl ():BytesData {
		return this;
	}

	inline function get_length ():Int {
		return BytesDataTools.getLength(this);
	}
}
