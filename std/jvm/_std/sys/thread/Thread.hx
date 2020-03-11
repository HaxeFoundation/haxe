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

import java.Lib;
import java.lang.Runnable;

abstract Thread(HaxeThread) {
	inline function new(t:HaxeThread) {
		this = t;
	}

	public static function create(callb:Void->Void):Thread {
		var haxeThread = new HaxeThread(new java.lang.Thread((cast callb : Runnable)));
		haxeThread.native.setDaemon(true);
		haxeThread.native.start();
		return new Thread(haxeThread);
	}

	public static function current():Thread {
		var nativeThread = java.lang.Thread.currentThread();
		Lib.lock(HaxeThread.threadMap, {
			var haxeThread = HaxeThread.threadMap.get(nativeThread);
			return new Thread(haxeThread);
		});
		return null;
	}

	public static function readMessage(block:Bool):Dynamic {
		return current().getHandle().messages.pop(block);
	}

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	private inline function getHandle():HaxeThread {
		return this;
	}
}

class HaxeThread {
	static public var threadMap = new haxe.ds.WeakMap<java.lang.Thread, HaxeThread>();

	@:keep
	static var mainHaxeThread = new HaxeThread(java.lang.Thread.currentThread());

	public var messages:Deque<Dynamic>;
	public var native:java.lang.Thread;

	public function new(native:java.lang.Thread) {
		this.native = native;
		this.messages = new Deque();
		Lib.lock(threadMap, threadMap.set(native, this));
	}

	public function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}
}
