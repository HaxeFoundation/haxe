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

private typedef ThreadHandle = hl.Abstract<"hl_thread">;

abstract Thread(ThreadHandle) from ThreadHandle to ThreadHandle {
	public function sendMessage(msg:Dynamic) {
		getQueue(this).add(msg);
	}

	public static function readMessage(block = true):Dynamic {
		return getQueue(cast current()).pop(block);
	}

	static var queue_mutex:Mutex = null;
	static var threads_queues:Array<{t:ThreadHandle, q:Deque<Dynamic>}> = null;

	static function getQueue(t:ThreadHandle) {
		if (queue_mutex == null) {
			queue_mutex = new Mutex();
			threads_queues = [];
		}
		queue_mutex.acquire();
		var q = null;
		for (tq in threads_queues)
			if (tq.t == t) {
				q = tq.q;
				break;
			}
		if (q == null) {
			q = new Deque<Dynamic>();
			threads_queues.push({t: t, q: q});
		}
		queue_mutex.release();
		return q;
	}

	static public function create(callb:()->Void):Thread {
		return createHandle(() -> {
			try {
				callb();
			} catch(e) {
				dropThread(current());
				throw e;
			}
			dropThread(current());
		});
	}

	static inline function dropThread(handle:ThreadHandle) {
		queue_mutex.acquire();
		for (i => tq in threads_queues) {
			if (tq.t == handle) {
				threads_queues.splice(i, 1);
				break;
			}
		}
		queue_mutex.release();
	}

	@:hlNative("std", "thread_create")
	static function createHandle(callb:()->Void):ThreadHandle {
		return null;
	}

	@:hlNative("std", "thread_current")
	public static function current():Thread {
		return null;
	}
}
