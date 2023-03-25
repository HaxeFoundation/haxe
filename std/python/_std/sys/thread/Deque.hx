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

import python.lib.threading.Condition;

using python.internal.UBuiltins;

class Deque<T> {
	var deque:NativeDeque<T>;
	var lock:Condition;

	public function new() {
		deque = new NativeDeque<T>();
		lock = new Condition();
	}

	public function add(i:T) {
		lock.acquire();
		deque.append(i);
		lock.notify();
		lock.release();
	}

	public function push(i:T) {
		lock.acquire();
		deque.appendleft(i);
		lock.notify();
		lock.release();
	}

	public function pop(block:Bool):Null<T> {
		var ret = null;
		lock.acquire();
		if (block) {
			lock.wait_for(() -> deque.bool());
			ret = deque.popleft();
		} else if (deque.bool()) {
			ret = deque.popleft();
		}
		lock.release();
		return ret;
	}
}

@:pythonImport("collections", "deque")
@:native("deque")
private extern class NativeDeque<T> {
	function new();
	function append(x:T):Void;
	function appendleft(x:T):Void;
	function popleft():T;
}
