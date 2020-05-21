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
	static function memInfo(inWhatInfo:Int):Float;

	@:native("_hx_allocate_extended") @:templatedCall
	static function allocateExtended<T>(cls:Class<T>, size:Int):T;

	@:native("_hx_add_finalizable")
	static function addFinalizable(instance:{function finalize():Void;}, inPin:Bool):Void;

	@:native("::hx::InternalNew")
	static function allocGcBytesRaw(inBytes:Int, isContainer:Bool):RawPointer<cpp.Void>;

	inline static function allocGcBytes(inBytes:Int):Pointer<cpp.Void> {
		return Pointer.fromRaw(allocGcBytesRaw(inBytes, false));
	}

	@:native("__hxcpp_enable") extern static function enable(inEnable:Bool):Void;

	@:native("__hxcpp_collect") extern static function run(major:Bool):Void;

	@:native("__hxcpp_gc_compact") extern static function compact():Void;

	@:native("__hxcpp_gc_trace") extern static function nativeTrace(sought:Class<Dynamic>, printInstances:Bool):Int;

	@:native("__hxcpp_gc_do_not_kill") extern static function doNotKill(inObject:Dynamic):Void;

	@:native("__hxcpp_get_next_zombie") extern static function getNextZombie():Dynamic;

	@:native("__hxcpp_gc_safe_point") extern static function safePoint():Void;

	@:native("__hxcpp_enter_gc_free_zone") extern static function enterGCFreeZone():Void;

	@:native("__hxcpp_exit_gc_free_zone") extern static function exitGCFreeZone():Void;

	@:native("__hxcpp_set_minimum_free_space") extern static function setMinimumFreeSpace(inBytes:Int):Void;

	@:native("__hxcpp_set_target_free_space_percentage") extern static function setTargetFreeSpacePercentage(inPercentage:Int):Void;

	@:native("__hxcpp_set_minimum_working_memory") extern static function setMinimumWorkingMemory(inBytes:Int):Void;
}
