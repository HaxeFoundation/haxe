/*
 * Copyright (C)2005-2019 Haxe Foundation
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

#if (hl_ver >= version("1.13.0") && !hl_legacy32)

typedef Int64MapData = Abstract<"hl_int64_map">;

abstract Int64Map(Int64MapData) {
	extern public inline function new() {
		this = alloc();
	}

	@:hlNative("std", "hi64alloc") static function alloc():Int64MapData {
		return null;
	}

	@:hlNative("std", "hi64set")
	public function set(key:haxe.Int64, value:Dynamic) {}

	@:hlNative("std", "hi64exists")
	public function exists(key:haxe.Int64):Bool {
		return false;
	}

	@:hlNative("std", "hi64get")
	public function get(key:haxe.Int64):Dynamic {
		return null;
	}

	@:hlNative("std", "hi64remove")
	public function remove(key:haxe.Int64):Bool {
		return false;
	}

	@:hlNative("std", "hi64keys")
	public function keysArray():NativeArray<haxe.Int64> {
		return null;
	}

	@:hlNative("std", "hi64values")
	public function valuesArray():NativeArray<Dynamic> {
		return null;
	}

	@:hlNative("std", "hi64clear")
	public function clear():Void {}

	extern public inline function iterator() {
		return new NativeArray.NativeArrayIterator<Dynamic>(valuesArray());
	}
}

#end
