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

package hl;

enum GcFlag {
	/**
		Activate profiling: regularly print on stdout gc allocation stats
	**/
	Profile;

	/**
		Allows one to dump a hlmemory.dump file when HL runs out of memory to be examined with hl memory inspector tool.
	**/
	DumpMem;

	/**
		Disable GC locking for multithreads
	**/
	NoThreads;

	/**
		Force major GC on each allocation
	**/
	ForceMajor;
}

class Gc {
	public static var flags(get, set):haxe.EnumFlags<GcFlag>;

	public static function stats() {
		var tot = 0., count = 0., mem = 0.;
		_stats(tot, count, mem);
		return {totalAllocated: tot, allocationCount: count, currentMemory: mem};
	}

	/**
		Dump whole memory into target filename for analysis.
	**/
	public static function dumpMemory(?fileName:String = "hlmemory.dump") {
		_dump(@:privateAccess fileName.toUtf8());
	}

	static function get_flags():haxe.EnumFlags<GcFlag> {
		return haxe.EnumFlags.ofInt(_get_flags());
	}

	static function set_flags(v:haxe.EnumFlags<GcFlag>) {
		_set_flags(v.toInt());
		return v;
	}

	#if (hl_ver >= version("1.15.0"))
	/**
		Count live objects of class `cl`, and get at most `maxCount` elements in an array.
	**/
	public static function getLiveObjects(cl:Class<Dynamic>, maxCount:Int = 0) {
		var arr = new hl.NativeArray<Dynamic>(maxCount);
		var count = _getLiveObjects(cast(cl,hl.BaseType).__type__, arr);
		var objs = new Array<Dynamic>();
		for (i in 0...maxCount) {
			if (arr[i] == null) break;
			objs.push(arr[i]);
		}
		return {count: count, objs: objs};
	}
	#end

	/**
		Enter/leave a blocking section: when in a blocking section the thread cannot
		allocate any memory but other threads will not wait for it for collecting memory.
	**/
	@:hlNative("std", "blocking")
	public static function blocking(b:Bool) {}

	@:hlNative("std", "gc_dump_memory") static function _dump(b:hl.Bytes):Void {}

	@:hlNative("std", "gc_enable") public static function enable(b:Bool):Void {}

	@:hlNative("std", "gc_major") public static function major():Void {}

	@:hlNative("std", "gc_stats") static function _stats(totalAllocated:hl.Ref<Float>, allocationCount:hl.Ref<Float>, currentMemory:hl.Ref<Float>):Void {}

	@:hlNative("std", "gc_get_flags") static function _get_flags():Int {
		return 0;
	}

	@:hlNative("std", "gc_set_flags") static function _set_flags(v:Int) {}

	#if (hl_ver >= version("1.15.0"))
	@:hlNative("std", "gc_get_live_objects") static function _getLiveObjects(type:hl.Type, arr:hl.NativeArray<Dynamic>):Int {
		return 0;
	}
	#end
}
