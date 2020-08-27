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
import java.util.concurrent.atomic.AtomicInteger;

@:coreApi
abstract Thread(NativeThread) {
	inline function new(t:NativeThread) {
		this = t;
	}

	public static function create(callb:Void->Void):Thread {
		var ret = new NativeThread();
		var t = new HaxeThread(ret, callb);
		t.start();
		return new Thread(ret);
	}

	public static function current():Thread {
		return new Thread(NativeThread.getThread(java.lang.Thread.currentThread()));
	}

	public static function readMessage(block:Bool):Dynamic {
		return current().getHandle().messages.pop(block);
	}

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
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

	private inline function getHandle():NativeThread {
		return this;
	}

	private static inline function processEvents():Void {
		current().getHandle().processEvents();
	}
}

@:native('haxe.java.vm.Thread') private class NativeThread {
	@:private static var javaThreadToHaxe = new haxe.ds.WeakMap<java.lang.Thread, NativeThread>();
	@:private static var mainJavaThread = java.lang.Thread.currentThread();
	@:private static var mainHaxeThread = {
		var ret = new NativeThread();
		javaThreadToHaxe.set(mainJavaThread, ret);
		ret;
	};

	public static function getThread(jt:java.lang.Thread):NativeThread {
		if (Std.isOfType(jt, HaxeThread)) {
			var t:HaxeThread = cast jt;
			return t.threadObject;
		} else if (jt == mainJavaThread) {
			return mainHaxeThread;
		} else {
			var ret = null;
			untyped __lock__(javaThreadToHaxe, {
				ret = javaThreadToHaxe.get(jt);
				if (ret == null) {
					ret = new NativeThread();
					javaThreadToHaxe.set(jt, ret);
				}
			});
			return ret;
		}
	}

	public var messages:Deque<Dynamic>;
	public var events:Deque<()->Void>;
	public var promisedEvents = new AtomicInteger();

	public function new() {
		this.messages = new Deque();
		this.events = new Deque();
	}

	public function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}

	public function processEvents() {
		while(true) {
			switch events.pop(threadObject.promisedEvents.intValue() > 0) {
				case null: break;
				case event: event();
			}
		}
	}
}

@:native('haxe.java.vm.HaxeThread')
private class HaxeThread extends java.lang.Thread {
	public var threadObject(default, null):NativeThread;

	private var runFunction:Void->Void;

	@:overload override public function run():Void {
		runFunction();
		threadObject.processEvents();
	}

	public function new(hxThread:NativeThread, run:Void->Void) {
		super();
		threadObject = hxThread;
		runFunction = run;
		setDaemon(true);
	}
}
