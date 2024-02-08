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

package sys.thread;

@:coreApi
class Mutex {
	var m:Dynamic;

	public function new():Void {
		m = mutex_create();
	}

	public function acquire():Void {
		mutex_acquire(m);
	}

	public function tryAcquire():Bool {
		return mutex_try(m);
	}

	public function release():Void {
		mutex_release(m);
	}

	static var mutex_create = neko.Lib.loadLazy("std", "mutex_create", 0);
	static var mutex_release = neko.Lib.loadLazy("std", "mutex_release", 1);
	static var mutex_acquire = neko.Lib.loadLazy("std", "mutex_acquire", 1);
	static var mutex_try = neko.Lib.loadLazy("std", "mutex_try", 1);
}
