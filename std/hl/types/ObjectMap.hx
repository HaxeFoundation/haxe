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

typedef ObjectMapData = Abstract<"hl_obj_map">;

abstract ObjectMap(ObjectMapData) {

	@:extern public inline function new() {
		this = alloc();
	}

	@:hlNative("std","hoalloc") static function alloc() : ObjectMapData {
		return null;
	}

	@:hlNative("std","hoset")
	public function set( key : Dynamic, value : Dynamic ) {
	}

	@:hlNative("std","hoexists")
	public function exists( key : Dynamic ) : Bool {
		return false;
	}

	@:hlNative("std","hoget")
	public function get( key : Dynamic ) : Dynamic {
		return null;
	}

	@:hlNative("std","horemove")
	public function remove( key : Dynamic ) : Bool {
		return false;
	}

	@:hlNative("std","hokeys")
	public function keysArray() : NativeArray<Dynamic> {
		return null;
	}

	@:hlNative("std","hovalues")
	public function valuesArray() : NativeArray<Dynamic> {
		return null;
	}

	@:extern public inline function iterator() {
		return new NativeArray.NativeArrayIterator<Dynamic>(valuesArray());
	}

}