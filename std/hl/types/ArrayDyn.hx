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
package hl.types;
import hl.types.ArrayBase;

class ArrayDynIterator {
	var a : ArrayBase;
	var len : Int;
	var pos : Int;
	public function new(a) {
		this.a = a;
		this.len = a.length;
		this.pos = 0;
	}
	public function hasNext() {
		return pos < len;
	}
	public function next() {
		return a.getDyn(pos++);
	}
}

@:keep
class ArrayDyn extends ArrayAccess {

	public var length(get,never) : Int;
	var array : ArrayBase;
	var allowReinterpret : Bool;

	public function new() {
		array = new ArrayObj<Dynamic>();
		allowReinterpret = true;
	}

	inline function get_length() return array.length;

	override function getDyn(i) {
		return array.getDyn(i);
	}

	override function setDyn(pos:Int, value:Dynamic) {
		array.setDyn(pos, value);
	}

	override function blit( pos : Int, src : ArrayAccess, srcpos : Int, len : Int ) : Void {
		array.blit(pos, src, srcpos, len);
	}

	public function concat( a : ArrayDyn ) : ArrayDyn {
		var a1 = array;
		var a2 = a.array;
		var alen = a1.length;
		var anew = new NativeArray<Dynamic>(alen + a2.length);
		for( i in 0...alen )
			anew[i] = a1.getDyn(i);
		for( i in 0...a2.length )
			anew[i+alen] = a2.getDyn(i);
		return alloc(ArrayObj.alloc(anew),true);
	}

	public function join( sep : String ) : String {
		return array.join(sep);
	}

	public function pop() : Null<Dynamic> {
		return array.popDyn();
	}

	public function push(x : Dynamic) : Int {
		return array.pushDyn(x);
	}

	public function reverse() : Void {
		array.reverse();
	}

	public function shift() : Null<Dynamic> {
		return array.shiftDyn();
	}

	public function slice( pos : Int, ?end : Int ) : ArrayDyn {
		return alloc(array.slice(pos,end),true);
	}

	public function sort( f : Dynamic -> Dynamic -> Int ) : Void {
		array.sortDyn(f);
	}

	public function splice( pos : Int, len : Int ) : ArrayDyn {
		return alloc(array.splice(pos,len),true);
	}

	public function toString() : String {
		return array.toString();
	}

	public function unshift( x : Dynamic ) : Void {
		array.unshiftDyn(x);
	}

	public function insert( pos : Int, x : Dynamic ) : Void {
		array.insertDyn(pos,x);
	}

	public function remove( x : Dynamic ) : Bool {
		return array.removeDyn(x);
	}

	public function indexOf( x : Dynamic, ?fromIndex:Int ) : Int {
		var i : Int = fromIndex;
		var length = length;
		var array = array;
		while( i < length ) {
			if( array.getDyn(i) == x )
				return i;
			i++;
		}
		return -1;
	}

	public function lastIndexOf( x : Dynamic, ?fromIndex:Int ) : Int {
		var len = length;
		var i:Int = fromIndex != null ? fromIndex : len - 1;
		if( i >= len )
			i = len - 1;
		else if( i  < 0 )
			i += len;
		while( i >= 0 ) {
			if( array.getDyn(i) == x )
				return i;
			i--;
		}
		return -1;
	}

	public function copy() : ArrayDyn {
		var a = new NativeArray<Dynamic>(length);
		for( i in 0...length )
			a[i] = array.getDyn(i);
		return alloc(ArrayObj.alloc(a),true);
	}

	public function iterator() : Iterator<Dynamic> {
		return new ArrayDynIterator(array);
	}

	public function map( f : Dynamic -> Dynamic ) : ArrayDyn {
		var a = new NativeArray<Dynamic>(length);
		for( i in 0...length )
			a[i] = f(array.getDyn(i));
		return alloc(ArrayObj.alloc(a),true);
	}

	public function filter( f : Dynamic -> Bool ) : ArrayDyn {
		var a = new ArrayObj<Dynamic>();
		for( i in 0...length ) {
			var v = array.getDyn(i);
			if( f(v) ) a.push(v);
		}
		return alloc(a,true);
	}

	function __get_field( fid : Int ) : Dynamic {
		if( fid == untyped $hash("length") )
			return length;
		return null;
	}

	function __cast( t : Type ) : Dynamic {
		if( t == Type.getDynamic(array) )
			return array;
		if( !allowReinterpret )
			return null;
		if( t == Type.get(new ArrayBytes.ArrayI32()) ) {
			var a : BytesAccess<Int> = null;
			a = new Bytes(array.length << a.sizeBits);
			for( i in 0...array.length )
				a[i] = array.getDyn(i);
			var arr = ArrayBase.allocI32(a, array.length);
			array = arr;
			allowReinterpret = false;
			return arr;
		}
		if( t == Type.get(new ArrayBytes.ArrayF64()) ) {
			var a : BytesAccess<Float> = null;
			a = new Bytes(array.length << a.sizeBits);
			for( i in 0...array.length )
				a[i] = array.getDyn(i);
			var arr = ArrayBase.allocF64(a, array.length);
			array = arr;
			allowReinterpret = false;
			return arr;
		}
		return null;
	}

	public static function alloc( a : ArrayBase, allowReinterpret = false ) : ArrayDyn {
		var arr : ArrayDyn = untyped $new(ArrayDyn);
		arr.array = a;
		arr.allowReinterpret = allowReinterpret;
		return arr;
	}

}
