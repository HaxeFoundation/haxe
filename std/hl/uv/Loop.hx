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

package hl.uv;

enum abstract LoopRunMode(Int) {
	var Default = 0;
	var Once = 1;
	var NoWait = 2;
}

abstract Loop(hl.Abstract<"uv_loop">) {
	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "loop_close_wrap" #else "loop_close" #end)
	public function close():Int {
		return 0;
	}

	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "run_wrap" #else "run" #end)
	public function run(mode:LoopRunMode):Int {
		return 0;
	}

	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "loop_alive_wrap" #else "loop_alive" #end)
	public function alive():Int {
		return 0;
	}

	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "stop_wrap" #else "stop" #end)
	public function stop():Void {}

	public static function getFromEventLoop(loop:haxe.EventLoop):Loop {
		if (@:privateAccess loop.nativeLoop == null) {
			if (loop == haxe.EventLoop.main)
				@:privateAccess loop.nativeLoop = new LoopWrapper(default_loop());
			else {
				#if (hl_ver < version("1.16.0"))
				throw "Using libUV multithread requires -D hl-ver=1.16.0";
				#else
				@:privateAccess loop.nativeLoop = new LoopWrapper(create());
				#end
			}
		}
		final wrapped:LoopWrapper = cast @:privateAccess loop.nativeLoop;
		return @:privateAccess wrapped.uvLoop;
	}

	public static function getCurrent():Loop {
		return getFromEventLoop(haxe.EventLoop.current);
	}

	public static function getDefault():Loop {
		return getFromEventLoop(haxe.EventLoop.main);
	}

	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "default_loop_wrap" #else "default_loop" #end)
	static function default_loop():Loop {
		return null;
	}

	#if (hl_ver >= version("1.16.0"))
	@:hlNative("uv", "create_loop") public static function create():Loop {
		return null;
	}
	#end

}

private class LoopWrapper {
	public final allowsReentrancy = false;
	final uvLoop:Loop;

	public function new(loop:Loop) {
		this.uvLoop = loop;
	}

	public function run() {
		uvLoop.run(NoWait);
	}

	public function close() {
		final result = uvLoop.close();
		if (result != 0)
			Sys.println("Some async handlers have not been closed");
	}

	public function isAlive() {
		return uvLoop.alive() > 0;
	}
}
