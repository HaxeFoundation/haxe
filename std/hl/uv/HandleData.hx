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

/**
	Base type for all libuv handle types.

	@see http://docs.libuv.org/en/v1.x/handle.html
**/
abstract HandleData(hl.Abstract<"uv_handle">) {
	/**
		Returns `true` if the handle is active, `false` otherwise.
	**/
	public inline function isActive():Bool
		return is_active() != 0;

	@:hlNative("uv", "is_active") function is_active():Int
		return 0;

	/**
		Returns `true` if the handle is closing or closed, `false` otherwise.
	**/
	public inline function isClosing():Bool
		return is_closing() != 0;

	@:hlNative("uv", "is_closing") function is_closing():Int
		return 0;

	/**
		Request handle to be closed.
		`callback` will be called asynchronously after this call.
		This MUST be called on each handle.
	**/
	@:hlNative("uv", "close_handle") public function close(?callback:()->Void):Void {}

	/**
		Reference the given handle.
		If a handle is already referenced calling this function again will have no effect.

		@see http://docs.libuv.org/en/v1.x/handle.html#reference-counting
	**/
	@:hlNative("uv", "ref") public function ref():Void {}

	/**
		Unreference the given handle.
		If a handle is not referenced calling this function again will have no effect.

		@see http://docs.libuv.org/en/v1.x/handle.html#reference-counting
	**/
	@:hlNative("uv", "unref") public function unref():Void {}

	/**
		Returns `true` if the handle is referenced, `false` otherwise.

		@see http://docs.libuv.org/en/v1.x/handle.html#reference-counting
	**/
	public function hasRef():Bool
		return has_ref() != 0;

	@:hlNative("uv", "has_ref") function has_ref():Int
		return 0;
}
