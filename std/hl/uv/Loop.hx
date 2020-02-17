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
	@:hlNative("uv", "loop_close") public function close():Int {
		return 0;
	}

	@:hlNative("uv", "run") public function run(mode:LoopRunMode):Int {
		return 0;
	}

	@:hlNative("uv", "loop_alive") public function alive():Int {
		return 0;
	}

	@:hlNative("uv", "stop") public function stop():Void {}

	public static function getDefault():Loop {
		var def = default_loop();
		if (loopEvent == null)
			loopEvent = haxe.MainLoop.add(function() {
				// if no more things to process, stop
				if (def.run(NoWait) == 0) {
					loopEvent.stop();
					loopEvent = null;
				}
			});
		return def;
	}

	@:hlNative("uv", "default_loop") static function default_loop():Loop {
		return null;
	}

	static var loopEvent:haxe.MainLoop.MainEvent;
}
