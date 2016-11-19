/*
 * Copyright (C)2005-2016 Haxe Foundation
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

class Gc {


	public static function stats() {
		var tot = 0., count = 0., mem = 0.;
		_stats(tot, count, mem);
		return { totalAllocated : tot, allocationCount : count, currentMemory : mem };
	}

	@:hlNative("std", "gc_profile") public static function activeProfileInfos( b : Bool ) : Void {}
	@:hlNative("std", "gc_enable") public static function enable( b : Bool ) : Void {}
	@:hlNative("std", "gc_major") public static function major() : Void {}
	@:hlNative("std", "gc_stats") static function _stats( totalAllocated : hl.Ref<Float>, allocationCount : hl.Ref<Float>, currentMemory : hl.Ref<Float> ) : Void {}

}
