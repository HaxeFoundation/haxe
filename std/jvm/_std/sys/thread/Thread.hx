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
import java.util.concurrent.atomic.AtomicInteger;
import java.lang.Thread as JavaThread;
import java.util.Collections;
import java.util.WeakHashMap;

abstract Thread(HaxeThread) from HaxeThread to java.lang.Thread {
	inline function new(t:HaxeThread) {
		this = t;
	}

	public static function create(callb:Void->Void):Thread {
		var ret = new HaxeThread((cast callb : Runnable));
		ret.setDaemon(true);
		ret.start();
		return new Thread(ret);
	}

	public static function current():Thread {
		return new Thread(Std.downcast(JavaThread.currentThread(), HaxeThread));
	}

	public static function readMessage(block:Bool):Dynamic {
		return current().getHandle().messages.pop(block);
	}

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	public inline function scheduleEvent(event:()->Void):Void {
		this.scheduleEvent(event);
	}

	public inline function schedulePromisedEvent(event:()->Void):Void {
		this.schedulePromisedEvent(event);
	}

	public inline function promiseEvent():Void {
		this.promiseEvent();
	}

	private inline function getHandle():HaxeThread {
		return this;
	}
}

private class HaxeThread extends java.lang.Thread {

	public final messages = new Deque<Dynamic>();
	public final events = new Deque<()->Void>();
	public final promisedEvents = new AtomicInteger();

	public inline function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}

	public function scheduleEvent(event:()->Void):Void {
		events.add(event);
	}

	public function schedulePromisedEvent(event:()->Void):Void {
		promisedEvents.decrementAndGet();
		events.add(event);
	}

	public function promiseEvent():Void {
		promisedEvents.incrementAndGet();
	}

	override overload public function run():Void {
		super.run();
		processEvents();
	}

	public function processEvents():Void {
		while(true) {
			switch events.pop(promisedEvents.intValue() > 0) {
				case null: break;
				case event: event();
			}
		}
	}
}

private class WrappedThread extends HaxeThread {
	static public final mainThread = new MainThread(JavaThread.currentThread());
	static final wrappedThreads = Collections.synchronizedMap(new WeakHashMap<JavaThread, HaxeThread>());

	public function wrap(javaThread:JavaThread):HaxeThread {
		if(Std.is(javaThread, HaxeThread)) {
			return cast javaThread;
		} else {
			switch wrappedThreads.get(javaThread) {
				case null:
					var hxThread = new WrappedThread(javaThread);
					wrappedThreads.put(javaThread, hxThread);
					return hxThread;
				case hxThread:
					return hxThread;
			}
		}
	}
}
