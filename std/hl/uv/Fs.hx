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

 enum abstract Event(Int) {
	var Rename = 1;
	var Change = 2;
}

@:hlNative("uv")
class Fs extends Handle {
	public function new(?loop : Loop, path : String, onContentChanged : Event -> Void) {
		if(loop == null)
			loop = Loop.getDefault();
		super(fs_start_wrap(loop, (e) -> onContentChanged(cast(e, Event)), @:privateAccess path.toUtf8()));
	}

	public function stop() {
		if(handle == null)
			return;
		fs_stop_wrap(handle);
	}

	static function fs_start_wrap(loop:Loop, cb : Int -> Void, path : hl.Bytes) : HandleData {
		return null;
	}

	static function fs_stop_wrap(handle:HandleData) : Bool {
		return false;
	}
}