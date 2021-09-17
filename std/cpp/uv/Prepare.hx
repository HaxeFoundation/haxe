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
	Prepare handles will run the given callback once per loop iteration, right
	before polling for i/o.

	@see http://docs.libuv.org/en/v1.x/prepare.html
**/
@:headerCode('#include "uv.h"')
class Prepare extends Handle {
	var uvPrepare:RawPointer<UvPrepareT>;
	var onPrepare:()->Void;

	function setupUvHandle() {
		uvPrepare = UvPrepareT.create();
		uvHandle = cast uvPrepare;
	}

	/**
		Create a prepare.
	**/
	static public function init(loop:Loop):Prepare {
		var prepare = new Prepare();
		UV.prepare_init(loop.uvLoop, prepare.uvPrepare).resolve();
		return prepare;
	}

	/**
		Start the handle with the given callback.
	**/
	public function start(callback:()->Void) {
		uvPrepare.prepare_start(Callable.fromStaticFunction(uvPrepareCb)).resolve();
		onPrepare = callback;
	}

	static function uvPrepareCb(uvPrepare:RawPointer<UvPrepareT>) {
		var prepare = Std.downcast(Handle.getHandle(cast uvPrepare), Prepare);
		prepare.onPrepare();
	}

	/**
		Stop the prepare
	**/
	public function stop() {
		UV.prepare_stop(uvPrepare).resolve();
		onPrepare = null;
	}
}