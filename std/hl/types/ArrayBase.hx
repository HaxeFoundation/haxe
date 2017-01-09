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

@:keep
class ArrayAccess {

	public function getDyn( pos : Int ) : Dynamic {
		throw "Not implemented";
		return 0;
	}

	public function setDyn( pos : Int, v : Dynamic ) {
		throw "Not implemented";
	}

	public function blit( pos : Int, src : ArrayAccess, srcpos : Int, len : Int ) : Void {
		throw "Not implemented";
	}

}

@:keep
class ArrayBase extends ArrayAccess {

	public var length(default,null) : Int;


	public function pushDyn( v : Dynamic ) : Int {
		throw "Not implemented";
		return 0;
	}

	public function popDyn() : Null<Dynamic> {
		throw "Not implemented";
		return null;
	}

	public function shiftDyn() : Null<Dynamic> {
		throw "Not implemented";
		return null;
	}

	public function unshiftDyn( v : Dynamic ) : Void {
		throw "Not implemented";
	}

	public function insertDyn( pos : Int, v : Dynamic ) : Void {
		throw "Not implemented";
	}

	public function removeDyn( v : Dynamic ) : Bool {
		throw "Not implemented";
		return false;
	}

	public function sortDyn( f : Dynamic -> Dynamic -> Int ) : Void {
		throw "Not implemented";
	}

	public function slice( pos : Int, ?end : Int ) : ArrayBase{
		throw "Not implemented";
		return null;
	}

	public function splice( pos : Int, len : Int ) : ArrayBase{
		throw "Not implemented";
		return null;
	}

	public function join( sep : String ) : String {
		throw "Not implemented";
		return null;
	}

	public function reverse() {
		throw "Not implemented";
	}

	public function toString() : String {
		throw "Not implemented";
		return null;
	}

	function __cast( t : Type ) : Dynamic {
		if( t == Type.get(new ArrayDyn()) )
			return ArrayDyn.alloc(this, false);
		return null;
	}

	function isArrayObj() {
		return false;
	}

	public static function allocI32( bytes : BytesAccess<Int>, length : Int ) @:privateAccess {
		var a : ArrayBytes.ArrayI32 = untyped $new(ArrayBytes.ArrayI32);
		a.length = length;
		a.bytes = bytes;
		a.size = length;
		return a;
	}

	public static function allocUI16( bytes : BytesAccess<UI16>, length : Int ) @:privateAccess {
		var a : ArrayBytes.ArrayUI16 = untyped $new(ArrayBytes.ArrayUI16);
		a.length = length;
		a.bytes = bytes;
		a.size = length;
		return a;
	}

	public static function allocF32( bytes : BytesAccess<F32>, length : Int ) @:privateAccess {
		var a : ArrayBytes.ArrayF32 = untyped $new(ArrayBytes.ArrayF32);
		a.length = length;
		a.bytes = bytes;
		a.size = length;
		return a;
	}

	public static function allocF64( bytes : BytesAccess<Float>, length : Int ) @:privateAccess {
		var a : ArrayBytes.ArrayF64 = untyped $new(ArrayBytes.ArrayF64);
		a.length = length;
		a.bytes = bytes;
		a.size = length;
		return a;
	}

}
