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

package js.node.fs;

import js.node.Fs.FsPath;
import js.node.events.EventEmitter.Event;

enum abstract WriteStreamEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		Emitted when the `WriteStream`'s file is opened.

		Listener arguments:
			fd - file descriptor used by the `WriteStream`.
	**/
	var Open:WriteStreamEvent<Int->Void> = "open";
}

/**
	Writable file stream.
**/
extern class WriteStream extends js.node.stream.Writable<WriteStream> {
	/**
		The path to the file the stream is writing to as specified in the first argument to `Fs.createWriteStream`.
		If path is passed as a string, then writeStream.path will be a string.
		If path is passed as a Buffer, then writeStream.path will be a Buffer.
	**/
	var path:FsPath;

	/**
		The number of bytes written so far.
		Does not include data that is still queued for writing.
	**/
	var bytesWritten:Int;
}
