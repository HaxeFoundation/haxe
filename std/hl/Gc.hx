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

enum GcFlag {
	/**
		Activate profiling: regularly print on stdout gc allocation stats
	**/
	Profile;
	/**
		Allows to dump a hlmemory.dump file when HL runs out of memory to be examined with hl memory inspector tool.
	**/
	DumpMem;
	/**
		Enable allocation tracking
	**/
	Track;
}

class Gc {

	public static var flags(get,set) : haxe.EnumFlags<GcFlag>;

	public static function stats() {
		var tot = 0., count = 0., mem = 0.;
		_stats(tot, count, mem);
		return { totalAllocated : tot, allocationCount : count, currentMemory : mem };
	}

	/**
		Dump whole memory into target filename for analysis.
	**/
	public static function dumpMemory( ?fileName : String = "hlmemory.dump" ) {
		_dump(@:privateAccess fileName.toUtf8());
	}

	static function get_flags() : haxe.EnumFlags<GcFlag> {
		return haxe.EnumFlags.ofInt(_get_flags());
	}

	static function set_flags(v : haxe.EnumFlags<GcFlag>) {
		_set_flags(v.toInt());
		return v;
	}

	@:hlNative("std", "gc_dump_memory") static function _dump( b : hl.Bytes ) : Void {}

	@:hlNative("std", "gc_enable") public static function enable( b : Bool ) : Void {}
	@:hlNative("std", "gc_major") public static function major() : Void {}
	@:hlNative("std", "gc_stats") static function _stats( totalAllocated : hl.Ref<Float>, allocationCount : hl.Ref<Float>, currentMemory : hl.Ref<Float> ) : Void {}

	@:hlNative("std", "gc_get_flags") static function _get_flags() : Int { return 0; }
	@:hlNative("std", "gc_set_flags") static function _set_flags( v : Int ) {}

}
