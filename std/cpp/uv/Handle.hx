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

enum abstract HandleType(Int) {
	var UNKNOWN_HANDLE = 0;
	var ASYNC;
	var CHECK;
	var FS_EVENT;
	var FS_POLL;
	var HANDLE;
	var IDLE;
	var NAMED_PIPE;
	var POLL;
	var PREPARE;
	var PROCESS;
	var STREAM;
	var TCP;
	var TIMER;
	var TTY;
	var UDP;
	var SIGNAL;
	var FILE;
}

/**
	Base type for all libuv handles.

	@see http://docs.libuv.org/en/v1.x/handle.html
**/
@:headerCode('#include "uv.h"')
abstract class Handle {
	var uvHandle:RawPointer<UvHandleT>;
	var onClose:()->Void;

	function new() {
		setupUvHandle();
		uvHandle.handle_set_data(untyped __cpp__('{0}.GetPtr()', this));
		cpp.vm.Gc.setFinalizer(this, Function.fromStaticFunction(finalizer));
	}

	static function getHandle(uvHandle:RawPointer<UvHandleT>):Handle {
		return untyped __cpp__('(hx::Object*){0}', uvHandle.handle_get_data());
	}

	abstract function setupUvHandle():Void;

	static function finalizer(handle:Handle) {
		// untyped __cpp__('printf("FINALIZER!\n")');
		Stdlib.free(Pointer.fromRaw(handle.uvHandle));
	}

	/**
		Returns `true` if the handle is active.
	**/
	public function isActive():Bool {
		return 0 != UV.is_active(uvHandle);
	}

	/**
		Returns `true` if the handle is closing or closed.
	**/
	public function isClosing():Bool {
		return 0 != UV.is_closing(uvHandle);
	}

	/**
		Request handle to be closed.

		This MUST be called on each handle.
	**/
	public function close(?callback:()->Void) {
		UV.close(uvHandle, Callable.fromStaticFunction(uvCloseCb));
		onClose = callback;
	}

	@:allow(cpp.uv.Tcp)
	static function uvCloseCb(uvHandle:RawPointer<UvHandleT>) {
		var handle = getHandle(uvHandle);
		uvHandle.handle_set_data(null);
		if(handle != null && handle.onClose != null) {
			handle.onClose();
		}
	}

	/**
		Reference the handle.

		@see http://docs.libuv.org/en/v1.x/handle.html#reference-counting
	**/
	public function ref() {
		UV.ref(uvHandle);
	}

	/**
		Un-reference the handle.

		@see http://docs.libuv.org/en/v1.x/handle.html#reference-counting
	**/
	public function unref() {
		UV.unref(uvHandle);
	}

	/**
		Returns `true` if the handle is referenced.

		@see http://docs.libuv.org/en/v1.x/handle.html#reference-counting
	**/
	public function hasRef() {
		return 0 != UV.has_ref(uvHandle);
	}
}