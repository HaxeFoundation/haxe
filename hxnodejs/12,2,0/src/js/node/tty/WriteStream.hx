/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node.tty;

import js.node.events.EventEmitter;

/**
	Enumeration of events emitted by `WriteStream` objects in addition to its parents.
**/
enum abstract WriteStreamEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		Emitted by refreshSize() when either of the columns or rows properties has changed.
	**/
	var Resize:WriteStreamEvent<Void->Void> = "resize";
}

/**
	A net.Socket subclass that represents the writable portion of a tty.
	In normal circumstances, process.stdout will be the only tty.WriteStream instance
	ever created (and only when isatty(1) is true).
**/
@:jsRequire("tty", "WriteStream")
extern class WriteStream extends js.node.net.Socket {
	/**
		The number of columns the TTY currently has.
		This property gets updated on "resize" events.
	**/
	var columns(default, null):Int;

	/**
		The number of rows the TTY currently has.
		This property gets updated on "resize" events.
	**/
	var rows(default, null):Int;
}
