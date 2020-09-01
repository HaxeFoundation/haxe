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

import eval.vm.NativeThread;

abstract Thread(NativeThread) {
	public var events(get,never):EventLoop;

	inline function new(h:NativeThread):Void {
		this = h;
	}

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	public static inline function current():Thread {
		return new Thread(NativeThread.self());
	}

	public static inline function create(callb:Void->Void):Thread {
		return new Thread(new NativeThread(() -> {
			callb();
			NativeThread.self().events().loop();
		}));
	}

	public static inline function readMessage(block:Bool):Dynamic {
		return NativeThread.readMessage(block);
	}

	public static inline function yield():Void {
		NativeThread.yield();
	}

	@:op(A == B)
	public inline function equals(other:Thread):Bool {
		return getHandle().id() == other.getHandle().id();
	}

	inline function getHandle():NativeThread {
		return this;
	}

	inline function get_events():EventLoop {
		return this.events();
	}

	@:keep
	static function processEvents():Void {
		current().getHandle().events().loop();
	}
}
