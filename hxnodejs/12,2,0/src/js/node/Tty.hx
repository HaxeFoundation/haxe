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

package js.node;

/**
	The tty module houses the tty.ReadStream and tty.WriteStream classes.
	In most cases, you will not need to use this module directly.

	When node detects that it is being run inside a TTY context, then process.stdin will be a tty.ReadStream
	instance and process.stdout will be a tty.WriteStream instance. The preferred way to check if node is being
	run in a TTY context is to check process.stdout.isTTY.
**/
@:jsRequire("tty")
extern class Tty {
	/**
		Returns true or false depending on if the `fd` is associated with a terminal.
	**/
	static function isatty(fd:Int):Bool;

	@:deprecated("Use tty.ReadStream#setRawMode() instead.")
	static function setRawMode(mode:Bool):Void;
}
