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
	Base type for all libuv request types.

	@see http://docs.libuv.org/en/v1.x/request.html
**/
@:headerCode('#include "uv.h"')
abstract class Request {
	var uvReq:RawPointer<UvReqT>;

	function new() {
		initUvReq();
		uvReq.req_set_data(untyped __cpp__('{0}.GetPtr()', this));
		cpp.vm.Gc.setFinalizer(this, Function.fromStaticFunction(finalizer));
	}

	static function getRequest(uvReq:RawPointer<UvReqT>):Request {
		return untyped __cpp__('(hx::Object*){0}', uvReq.req_get_data());
	}

	abstract function setupUvReq():Void;

	static function finalizer(handle:Request) {
		handle.destructor();
	}

	function destructor() {
		Stdlib.free(Pointer.fromRaw(handle.uvReq));
	}

	/**
		Cancel a pending request.
		Fails if the request is executing or has finished executing.
	**/
	public function cancel() {
		uvReq.cancel().resolve();
	}
}