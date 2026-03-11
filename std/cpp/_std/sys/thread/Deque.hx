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
class Deque<T> {
	final storage:Array<T>;
	final condition:Condition;

	public function new() {
		storage   = [];
		condition = new Condition();
	}

	public function add(i:T):Void {
		condition.acquire();
		storage.push(i);
		condition.signal();
		condition.release();
	}

	public function push(i:T):Void {
		condition.acquire();
		storage.insert(0, i);
		condition.signal();
		condition.release();
	}

	public function pop(block:Bool):Null<T> {
		if (block)
		{
			condition.acquire();

			var result = storage.shift();
			while (null == result) {
				condition.wait();

				result = storage.shift();
			}

			condition.release();

			return result;
		}
		else
		{
			condition.acquire();
			final result = storage.shift();
			condition.release();
			
			return result;
		}
	}
}
