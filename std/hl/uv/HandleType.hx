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
	The kind of the libuv handle.

	@see http://docs.libuv.org/en/v1.x/handle.html#c.uv_handle_type

	TODO: use native values.
**/
enum abstract HandleType(Int) {
	var UV_UNKNOWN_HANDLE = 0;
	var UV_ASYNC = 1;
	var UV_CHECK = 2;
	var UV_FS_EVENT = 3;
	var UV_FS_POLL = 4;
	var UV_HANDLE = 5;
	var UV_IDLE = 6;
	var UV_NAMED_PIPE = 7;
	var UV_POLL = 8;
	var UV_PREPARE = 9;
	var UV_PROCESS = 10;
	var UV_STREAM = 11;
	var UV_TCP = 12;
	var UV_TIMER = 13;
	var UV_TTY = 14;
	var UV_UDP = 15;
	var UV_SIGNAL = 16;
	var UV_FILE = 17;
}