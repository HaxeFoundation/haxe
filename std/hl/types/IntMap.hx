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

typedef IntMapData = Abstract<"hl_int_map">;

abstract IntMap(IntMapData) {

	@:extern public inline function new() {
		this = alloc();
	}

	@:hlNative("std","hialloc") static function alloc() : IntMapData {
		return null;
	}

	@:hlNative("std","hiset")
	public function set( key : Int, value : Dynamic ) {
	}

	@:hlNative("std","hiexists")
	public function exists( key : Int ) : Bool {
		return false;
	}

	@:hlNative("std","higet")
	public function get( key : Int ) : Dynamic {
		return null;
	}

	@:hlNative("std","hiremove")
	public function remove( key : Int ) : Bool {
		return false;
	}

	@:hlNative("std","hikeys")
	public function keysArray() : NativeArray<Int> {
		return null;
	}

	@:hlNative("std","hivalues")
	public function valuesArray() : NativeArray<Dynamic> {
		return null;
	}

	@:extern public inline function iterator() {
		return new NativeArray.NativeArrayIterator<Dynamic>(valuesArray());
	}

}