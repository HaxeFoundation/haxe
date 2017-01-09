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
package hl;

@:generic class NativeArrayIterator<T> {
	var arr : NativeArray<T>;
	var pos : Int;
	var length : Int;

	public inline function new(arr:NativeArray<T>) {
		this.arr = arr;
		pos = 0;
		length = arr.length;
	}

	public inline function hasNext() {
		return pos < length;
	}

	public inline function next() {
		return arr[pos++];
	}
}

@:coreType abstract NativeArray<T> {

	public var length(get,never):Int;

	@:extern public inline function new( length : Int ) {
		this = untyped $aalloc(length);
	}

	@:extern inline function get_length() : Int {
		return untyped $asize(this);
	}

	@:extern @:arrayAccess inline function get( pos : Int ) : T {
		return untyped ($aget(this,pos):T);
	}

	@:extern @:arrayAccess inline function set( pos : Int, value : T ) : T {
		untyped $aset(this,pos,value);
		return value;
	}

	@:extern public inline function sub( pos : Int, len : Int ) {
		var n = new NativeArray<T>(len);
		n.blit(0, this, pos, len);
		return n;
	}

	@:hlNative("std","array_type") public function getType() : Type {
		return null;
	}

 	@:hlNative("std","array_blit") public function blit( pos : Int, src : NativeArray<T>, srcPos : Int, srcLen : Int ) : Void {
	}

}