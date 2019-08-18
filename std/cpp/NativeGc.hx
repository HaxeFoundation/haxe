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

package cpp;

extern class NativeGc {
	@:native("__hxcpp_gc_mem_info")
	static public function memInfo(inWhatInfo:Int):Float;

	@:native("_hx_allocate_extended") @:templatedCall
	static public function allocateExtended<T>(cls:Class<T>, size:Int):T;

	@:native("_hx_add_finalizable")
	public static function addFinalizable(instance:{function finalize():Void;}, inPin:Bool):Void;

	@:native("hx::InternalNew")
	public static function allocGcBytesRaw(inBytes:Int, isContainer:Bool):RawPointer<cpp.Void>;

	inline public static function allocGcBytes(inBytes:Int):Pointer<cpp.Void> {
		return Pointer.fromRaw(allocGcBytesRaw(inBytes, false));
	}

	@:native("__hxcpp_enable") extern static public function enable(inEnable:Bool):Void;

	@:native("__hxcpp_collect") extern static public function run(major:Bool):Void;

	@:native("__hxcpp_gc_compact") extern static public function compact():Void;

	@:native("__hxcpp_gc_trace") extern static public function nativeTrace(sought:Class<Dynamic>, printInstances:Bool):Int;

	@:native("__hxcpp_gc_do_not_kill") extern static public function doNotKill(inObject:Dynamic):Void;

	@:native("__hxcpp_get_next_zombie") extern static public function getNextZombie():Dynamic;

	@:native("__hxcpp_gc_safe_point") extern static public function safePoint():Void;

	@:native("__hxcpp_enter_gc_free_zone") extern static public function enterGCFreeZone():Void;

	@:native("__hxcpp_exit_gc_free_zone") extern static public function exitGCFreeZone():Void;

	@:native("__hxcpp_set_minimum_free_space") extern static public function setMinimumFreeSpace(inBytes:Int):Void;

	@:native("__hxcpp_set_target_free_space_percentage") extern static public function setTargetFreeSpacePercentage(inPercentage:Int):Void;

	@:native("__hxcpp_set_minimum_working_memory") extern static public function setMinimumWorkingMemory(inBytes:Int):Void;
}
