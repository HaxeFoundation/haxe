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


#if (hl_ver >= version("1.11.0"))

typedef LockHandle = hl.Abstract<"hl_lock">;

@:coreApi
@:hlNative("std")
class Lock {
	var handle : LockHandle;

	public function new() {
		handle = lock_create();
	}

	public function wait( ?timeout : Float ) : Bool {
		return lock_wait(handle, timeout);
	}

	public function release( ) : Void {
		lock_release(handle);
	}

	static function lock_wait( handle : LockHandle, ?timeout : Float ) : Bool {
		return false;
	}

	static function lock_release( handle : LockHandle ) : Void { }

	static function lock_create( ) : LockHandle {
		return null;
	}
}

#else

@:coreApi
class Lock {
	var deque:sys.thread.Deque<Bool>;

	public function new():Void {
		deque = new Deque<Null<Bool>>();
	}

	public function wait(?timeout:Float):Bool {
		if (timeout == null) {
			deque.pop(true);
			return true;
		}
		var targetTime = haxe.Timer.stamp() + timeout;
		do {
			if (deque.pop(false) != null) {
				return true;
			}
		} while (haxe.Timer.stamp() < targetTime);
		return false;
	}

	public function release():Void {
		deque.push(true);
	}
}

#end