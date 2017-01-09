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
package haxe.ds;

private typedef VectorData<T> = Array<T>

@:coreApi
abstract Vector<T>(VectorData<T>) {

	public inline function new(length:Int) {
		this = [];
		if( length > 0 ) this[length-1] = cast null;
	}

	@:op([]) public inline function get(index:Int):T {
		return this[index];
	}

	@:op([]) public inline function set(index:Int, val:T):T {
		return this[index] = val;
	}

	public var length(get, never):Int;

	inline function get_length():Int {
		return this.length;
	}

	public static inline function blit<T>(src:Vector<T>, srcPos:Int, dest:Vector<T>, destPos:Int, len:Int):Void {
		(cast dest : hl.types.ArrayBase.ArrayAccess).blit(destPos,(cast src : hl.types.ArrayBase.ArrayAccess),srcPos,len);
	}

	public inline function toArray():Array<T> {
		return this.copy();
	}

	public inline function toData():VectorData<T>
		return this;

	static public inline function fromData<T>(data:VectorData<T>):Vector<T>
		return cast data;

	static public inline function fromArrayCopy<T>(array:Array<T>):Vector<T> {
		return cast array.copy();
	}

	public inline function copy<T>():Vector<T> {
		return cast this.copy();
	}

	public inline function join<T>(sep:String):String {
		return this.join(sep);
	}

	public inline function sort<T>(f:T->T->Int):Void {
		this.sort(f);
	}

	public inline function map<S>(f:T->S):Vector<S> {
		var length = length;
		var r = new Vector<S>(length);
		var i = 0;
		var len = length;
		for(i in 0...len) {
			var v = f(get(i));
			r.set(i, v);
		}
		return r;
	}
}
