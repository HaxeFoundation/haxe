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
import java.lang.System;
import java.StdTypes.Int64 as Long;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.LinkedBlockingDeque;

private typedef ThreadImpl = HaxeThread;

abstract Thread(ThreadImpl) from ThreadImpl {
	public var events(get,never):EventLoop;

	inline function new(t:HaxeThread) {
		this = t;
	}

	public static inline function create(job:()->Void):Thread {
		return HaxeThread.create(job, false);
	}

	public static inline function current():Thread {
		return HaxeThread.get(JavaThread.currentThread());
	}

	public static inline function runWithEventLoop(job:()->Void):Void {
		HaxeThread.runWithEventLoop(job);
	}

	public static inline function createWithEventLoop(job:()->Void):Thread {
		return HaxeThread.create(job, true);
	}

	public static inline function readMessage(block:Bool):Dynamic {
		return current().getHandle().readMessage(block);
	}

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	inline function getHandle():HaxeThread {
		return this;
	}

	function get_events():EventLoop {
		if(this.events == null)
			throw new NoEventLoopException();
		return this.events;
	}

	@:keep //TODO: keep only if events are actually used
	static function processEvents():Void {
		current().getHandle().events.loop();
	}
}

private class HaxeThread {
	static var nativeThreads:java.util.Map<JavaThread,HaxeThread>;
	static var mainJavaThread:JavaThread;
	static var mainHaxeThread:HaxeThread;

	static function __init__() {
		nativeThreads = Collections.synchronizedMap(new WeakHashMap<JavaThread,HaxeThread>());
		mainJavaThread = JavaThread.currentThread();
		mainHaxeThread = new HaxeThread();
		mainHaxeThread.events = new EventLoop();
	}

	public final messages = new LinkedBlockingDeque<Dynamic>();

	public var events(default,null):Null<EventLoop>;

	public static function create(job:()->Void, withEventLoop:Bool):HaxeThread {
		var hx = new HaxeThread();
		if(withEventLoop)
			hx.events = new EventLoop();
		var thread = new NativeHaxeThread(hx, job, withEventLoop);
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

	public static function runWithEventLoop(job:()->Void):Void {
		var thread = get(JavaThread.currentThread());
		if(thread.events == null) {
			thread.events = new EventLoop();
			try {
				job();
				thread.events.loop();
				thread.events = null;
			} catch(e) {
				thread.events = null;
				throw e;
			}
		} else {
			job();
		}
	}

	function new() {}

	public function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}

	public function readMessage(block:Bool):Dynamic {
		return block ? messages.take() : messages.poll();
	}
}

private class NativeHaxeThread extends java.lang.Thread {
	public final haxeThread:HaxeThread;
	final withEventLoop:Bool;

	public function new(haxeThread:HaxeThread, job:()->Void, withEventLoop:Bool) {
		super(new Job(job));
		this.haxeThread = haxeThread;
		this.withEventLoop = withEventLoop;
	}

	override overload public function run() {
		super.run();
		if(withEventLoop)
			haxeThread.events.loop();
	}
}

#if jvm
private abstract Job(Runnable) from Runnable to Runnable {
	public inline function new(job:()->Void) {
		this = cast job;
	}
}
#else
private class Job implements Runnable {
	final job:()->Void;

	public function new(job:()->Void) {
		this.job = job;
	}

	public function run() {
		job();
	}
}
#end