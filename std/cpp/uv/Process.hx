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

import cpp.uv.Signal;

using cpp.uv.UV;


/**
	Process handles will spawn a new process and allow the user to control it and
	establish communication channels with it using streams.

	@see http://docs.libuv.org/en/v1.x/process.html
**/
@:headerCode('#include "uv.h"')
class Process extends Handle {
	var uvSignal:Star<UvSignalT>;

	function initUvHandle() {
		uvSignal = UvSignalT.create();
		uvHandle = cast uvSignal;
	}

	/**
		Sends the specified signal to the given PID.
	**/
	static public function killPid(pid:Int, sigNum:SigNum) {
		UV.kill(pid, sigNum.toInt()).resolve();
	}
}