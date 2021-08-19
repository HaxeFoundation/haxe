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

enum abstract RequestType(Int) {
	var UV_UNKNOWN_REQ = 0;
	var UV_REQ;
	var UV_CONNECT;
	var UV_WRITE;
	var UV_SHUTDOWN;
	var UV_UDP_SEND;
	var UV_FS;
	var UV_WORK;
	var UV_GETADDRINFO;
	var UV_GETNAMEINFO;
	var UV_REQ_TYPE_MAX;
}

@:keep
@:allow(hl.uv)
abstract class RequestData {}

/**
	Base type for all libuv request types.

	@see http://docs.libuv.org/en/v1.x/request.html
**/
abstract Request(hl.Abstract<"uv_req">) {
	var req(get,never):Request;
	inline function get_req():Request return cast this;

	/**
		Cancel a pending request.

		Fails if the request is executing or has finished executing.
	**/
	public function cancel():Void {
		UV.cancel(req).resolve();
	}

	@:allow(hl.uv) inline function setData(data:RequestData)
		req.req_set_data_with_gc(data);

	@:allow(hl.uv) inline function getData():RequestData
		return req.req_get_data().req_data_of_pointer();
}
