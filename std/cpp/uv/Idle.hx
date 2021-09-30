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

package cpp.uv;

using cpp.uv.UV;

/**
	Idle handles will run the given callback once per loop iteration, right before
	the `cpp.uv.Prepare` handles.

	@see http://docs.libuv.org/en/v1.x/idle.html
**/
@:headerCode('#include "uv.h"')
class Idle extends Handle {
	var onIdle:()->Void;
	var uvIdle(get,never):RawPointer<UvIdleT>;

	inline function get_uvIdle():RawPointer<UvIdleT>
		return cast uv;

	override function setupUvData() {
		uv = cast UvIdleT.create();
		super.setupUvData();
	}

	/**
		Create a idle.
	**/
	static public function init(loop:Loop):Idle {
		var idle = new Idle();
		UV.idle_init(loop.uvLoop, idle.uvIdle).resolve();
		return idle;
	}

	/**
		Start the handle with the given callback.
	**/
	public function start(callback:()->Void) {
		uvIdle.idle_start(Callable.fromStaticFunction(uvIdleCb)).resolve();
		onIdle = callback;
	}

	static function uvIdleCb(uvIdle:RawPointer<UvIdleT>) {
		var idle:Idle = cast Handle.get(cast uvIdle);
		idle.onIdle();
	}

	/**
		Stop the idle.
	**/
	public function stop() {
		UV.idle_stop(uvIdle).resolve();
		onIdle = null;
	}
}