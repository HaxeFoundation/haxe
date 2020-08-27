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
import java.util.WeakHashMap;
import java.util.Collections;
import java.lang.Thread as JavaThread;
import java.util.concurrent.atomic.AtomicInteger;

abstract Thread(HaxeThread) from HaxeThread {
	inline function new(t:HaxeThread) {
		this = t;
	}

	public static inline function create(callb:()->Void):Thread {
		return HaxeThread.create(callb);
	}

	public static inline function current():Thread {
		return HaxeThread.get(JavaThread.currentThread());
	}

	public static inline function readMessage(block:Bool):Dynamic {
		return current().getHandle().readMessage(block);
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

	inline function getHandle():HaxeThread {
		return this;
	}

	private static inline function processEvents():Void {
		current().getHandle().processEvents();
	}
}

private class HaxeThread {
	static final nativeThreads = Collections.synchronizedMap(new WeakHashMap<JavaThread,HaxeThread>());
	static final mainJavaThread = JavaThread.currentThread();
	static final mainHaxeThread = new HaxeThread();

	public final messages = new Deque<Dynamic>();
	final events = new Deque<()->Void>();
	final promisedEvents = new AtomicInteger();

	public static function create(callb:()->Void):HaxeThread {
		var hx = new HaxeThread();
		var thread = new NativeHaxeThread(hx, callb);
		thread.setDaemon(true);
		thread.start();
		return hx;
	}

	public static function get(javaThread:JavaThread):HaxeThread {
		if(javaThread == mainJavaThread) {
			return mainHaxeThread;
		} else if(javaThread is NativeHaxeThread) {
			return (cast javaThread:NativeHaxeThread).haxeThread;
		} else {
			switch nativeThreads.get(javaThread) {
				case null:
					var hx = new HaxeThread();
					nativeThreads.put(javaThread, hx);
					return hx;
				case hx:
					return hx;
			}
		}
	}

	function new() {}

	public inline function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}

	public inline function readMessage(block:Bool):Dynamic {
		return messages.pop(block);
	}

	public function scheduleEvent(event:()->Void):Void {
		this.events.add(event);
	}

	public function schedulePromisedEvent(event:()->Void):Void {
		this.events.add(event);
		this.promisedEvents.decrementAndGet();
	}

	public function promiseEvent():Void {
		this.promisedEvents.incrementAndGet();
	}

	public function processEvents() {
		while(true) {
			switch events.pop(promisedEvents.intValue() > 0) {
				case null: break;
				case event: event();
			}
		}
	}
}

private class NativeHaxeThread extends java.lang.Thread {
	public final haxeThread:HaxeThread;

	public function new(haxeThread:HaxeThread, callb:()->Void) {
		super((cast callb:Runnable));
		this.haxeThread = haxeThread;
	}

	override overload public function run() {
		super.run();
		haxeThread.processEvents();
	}
}
