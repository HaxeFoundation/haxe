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

import cs.system.threading.Thread as NativeThread;
import cs.system.WeakReference;
import cs.Lib;

abstract Thread(HaxeThread) {
	inline function new(thread:HaxeThread) {
		this = thread;
	}

	public static function create(cb:Void->Void):Thread {
		var native = new NativeThread(cb);
		native.IsBackground = true;
		var hx = HaxeThread.allocate(native);
		native.Start();

		return new Thread(hx);
	}

	public static inline function current():Thread {
		return new Thread(HaxeThread.get(NativeThread.CurrentThread));
	}

	public static function readMessage(block:Bool):Dynamic {
		return current().readMessageImpl(block);
	}

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	inline function readMessageImpl(block:Bool):Dynamic {
		return this.readMessage(block);
	}
}

private class HaxeThread {
	static final threads = new Map<Int, WeakReference>();
	static var allocateCount = 0;

	public final native:NativeThread;

	final messages = new Deque<Dynamic>();

	public static function get(native:NativeThread):HaxeThread {
		var native = NativeThread.CurrentThread;
		var ref:Null<WeakReference> = null;
		Lib.lock(threads, {
			var key = native.ManagedThreadId;
			ref = threads.get(key);
		});
		if (ref == null || !ref.IsAlive) {
			return allocate(native);
		}
		return ref.Target;
	}

	public static function allocate(native:NativeThread):HaxeThread {
		allocateCount++;
		inline function cleanup() {
			if (allocateCount % 100 == 0) {
				for (key => ref in threads) {
					if (!ref.IsAlive) {
						threads.remove(key);
					}
				}
			}
		}
		var hx = new HaxeThread(native);
		var ref = new WeakReference(hx);
		Lib.lock(threads, {
			cleanup();
			threads.set(native.ManagedThreadId, ref);
		});
		return hx;
	}

	public function new(native:NativeThread) {
		this.native = native;
	}

	public inline function readMessage(block:Bool):Dynamic {
		return messages.pop(block);
	}

	public function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}
}
