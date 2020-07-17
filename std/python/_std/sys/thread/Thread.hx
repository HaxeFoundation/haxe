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

class Thread {
	var nativeThread: NativeThread;
	var messages: Deque<Dynamic>;

	static var threads = new haxe.ds.ObjectMap<NativeThread, Thread>();

	private function new(t:NativeThread) {
		nativeThread = t;
		messages = new Deque<Dynamic>();
	}

	public function sendMessage(msg:Dynamic):Void {
		trace('sending message $msg');
		messages.add(msg);
	}

	public static function current():Thread {
		var ct = PyThreadingAPI.current_thread();
		if (!threads.exists(ct)) threads.set(ct, new Thread(ct));
		return threads.get(ct);
	}

	public static function create(callb:Void->Void):Thread {
		var nt = new NativeThread(null, callb);
		nt.start();
		var t = new Thread(nt);
		threads.set(nt, t);
		return t;
	}

	public static function readMessage(block:Bool):Dynamic {
		trace("readMessage called");
		var msg = current().messages.pop(block);
		trace('readMessage finished with $msg');
		return msg;
	}
}

@:pythonImport("threading", "Thread")
@:native("Thread")
private extern class NativeThread {
	function new(group:Dynamic, target:Void->Void);
	function start():Void;
}

@:pythonImport("threading")
@:native("threading")
private extern class PyThreadingAPI {
	static function current_thread():NativeThread;
}