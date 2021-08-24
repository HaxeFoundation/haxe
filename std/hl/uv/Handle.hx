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

enum abstract HandleType(Int) {
	var UV_UNKNOWN_HANDLE = 0;
	var UV_ASYNC;
	var UV_CHECK;
	var UV_FS_EVENT;
	var UV_FS_POLL;
	var UV_HANDLE;
	var UV_IDLE;
	var UV_NAMED_PIPE;
	var UV_POLL;
	var UV_PREPARE;
	var UV_PROCESS;
	var UV_STREAM;
	var UV_TCP;
	var UV_TIMER;
	var UV_TTY;
	var UV_UDP;
	var UV_SIGNAL;
	var UV_FILE;
	var UV_HANDLE_TYPE_MAX;
}

/**
	Base type for all libuv handle types.

	@see http://docs.libuv.org/en/v1.x/handle.html
**/
abstract class Handle<T:RefUvHandleT> {
	var _h:RefUvHandleT;
	var onClose:()->Void;
	@:allow(hl.uv) var h(get,never):T;

	inline function get_h():T
		return (cast _h:T);

	function new(handle:T) {
		handle.handle_set_data_with_gc(this);
		_h = handle;
	}

	extern inline function handle(action:(h:T)->Void) {
		switch h {
			case null: throw new UVException(UV_EINVAL);
			case h: action(h);
		}
	}

	extern inline function handleReturn<R>(action:(h:T)->R) {
		return switch h {
			case null: throw new UVException(UV_EINVAL);
			case h: action(h);
		}
	}

	@:allow(hl.uv) inline function freeHandle():Void {
		_h.handle_set_data_with_gc(null);
		_h.handle_to_pointer().free();
		_h = null;
	}

	/**
		Returns `true` if the handle is active, `false` otherwise.
	**/
	public function isActive():Bool {
		return handleReturn(h -> h.is_active() != 0);
	}

	/**
		Returns `true` if the handle is closing or closed, `false` otherwise.
	**/
	public function isClosing():Bool
		return handleReturn(h -> h.is_closing() != 0);

	/**
		Request handle to be closed.
		`callback` will be called asynchronously after this call.
		This MUST be called on each handle.
	**/
	public function close(?callback:()->Void):Void
		handle(h -> {
			if(h.is_closing() != 0)
				throw new UVException(UV_EINVAL);
			onClose = () -> {
				freeHandle();
				if(callback != null)
					callback();
			};
			h.close_with_cb();
		});

	/**
		Reference the given handle.
		If a handle is already referenced calling this function again will have no effect.

		@see http://docs.libuv.org/en/v1.x/handle.html#reference-counting
	**/
	public function ref():Void
		handle(h -> UV.ref(h));

	/**
		Unreference the given handle.
		If a handle is not referenced calling this function again will have no effect.

		@see http://docs.libuv.org/en/v1.x/handle.html#reference-counting
	**/
	public function unref():Void
		handle(h -> UV.unref(h));

	/**
		Returns `true` if the handle is referenced, `false` otherwise.

		@see http://docs.libuv.org/en/v1.x/handle.html#reference-counting
	**/
	public function hasRef():Bool {
		return handleReturn(h -> h.has_ref() != 0);
	}
}
