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

abstract Thread(HaxeThread) from HaxeThread {
	inline function new(t:HaxeThread) {
		this = t;
	}

	public static inline function create(job:()->Void):Thread {
		return HaxeThread.create(job);
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

	public static inline function repeatEvent(event:()->Void, intervalMs:Int):EventHandler {
		return current().getHandle().repeatEvent(event, intervalMs);
	}

	public static inline function cancelEvent(eventHandler:EventHandler):Void {
		current().getHandle().cancelEvent(eventHandler);
	}

	public inline function runEvent(event:()->Void):Void {
		this.runEvent(event);
	}

	public inline function runPromisedEvent(event:()->Void):Void {
		this.runPromisedEvent(event);
	}

	public inline function promiseEvent():Void {
		this.promiseEvent();
	}

	inline function getHandle():HaxeThread {
		return this;
	}

	@:keep
	private static function processEvents():Void {
		current().getHandle().processEvents();
	}
}

abstract EventHandler(RegularEvent) from RegularEvent {
	inline function get():RegularEvent {
		return this;
	}
}

private class HaxeThread {
	static final nativeThreads = Collections.synchronizedMap(new WeakHashMap<JavaThread,HaxeThread>());
	static final mainJavaThread = JavaThread.currentThread();
	static final mainHaxeThread = new HaxeThread();

	public final messages = new LinkedBlockingDeque<Dynamic>();
	final oneTimeEvents = new LinkedBlockingDeque<()->Void>();
	final promisedEvents = new AtomicInteger();
	var regularEvents:Null<RegularEvent>;

	public static function create(job:()->Void):HaxeThread {
		var hx = new HaxeThread();
		var thread = new NativeHaxeThread(hx, job);
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

	public function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}

	public function readMessage(block:Bool):Dynamic {
		return block ? messages.take() : messages.poll();
	}

	public function repeatEvent(event:()->Void, intervalMs:Int):EventHandler {
		var event = new RegularEvent(event, System.currentTimeMillis() + intervalMs, intervalMs);
		switch regularEvents {
			case null:
			case current:
				event.next = current;
				current.previous = event;
		}
		regularEvents = event;
		return event;
	}

	public function cancelEvent(eventHandler:EventHandler):Void {
		var event = @:privateAccess eventHandler.get();
		if(regularEvents == eventHandler) {
			regularEvents = event.next;
		}
		switch event.next {
			case null:
			case e: e.previous = event.previous;
		}
		switch event.previous {
			case null:
			case e: e.next = event.next;
		}
	}

	public function runEvent(event:()->Void):Void {
		this.oneTimeEvents.add(event);
	}

	public function runPromisedEvent(event:()->Void):Void {
		this.oneTimeEvents.add(event);
		this.promisedEvents.decrementAndGet();
	}

	public function promiseEvent():Void {
		this.promisedEvents.incrementAndGet();
	}

	public function processEvents() {
		var eventsToRun = [];
		var eventsToRunIdx;
		var nextRegularIn:Long;
		while(true) {
			// How much time left till the next regular event
			nextRegularIn = -1;
			eventsToRunIdx = 0;
			// Collect regular events to run
			var current = regularEvents;
			var now = System.currentTimeMillis();
			while(current != null) {
				if(current.nextRunTime <= now) {
					eventsToRun[eventsToRunIdx++] = current.run;
					current.nextRunTime += current.interval;
					nextRegularIn = 0;
				} else if(nextRegularIn < 0 || current.nextRunTime - now < nextRegularIn) {
					nextRegularIn = current.nextRunTime - now;
				}
				current = current.next;
			}

			// Run regular events
			for(i in 0...eventsToRunIdx) {
				eventsToRun[i]();
				eventsToRun[i] = null;
			}
			eventsToRunIdx = 0;

			// Collect pending one-time events
			while(true) {
				switch oneTimeEvents.poll() {
					case null: break;
					case event: eventsToRun[eventsToRunIdx++] = event;
				}
			}

			// No events ready to run. Check if any events are expected to come.
			if(eventsToRunIdx == 0 && promisedEvents.intValue() > 0) {
				// If no more regular events are scheduled, then wait for a promised event infinitely.
				switch nextRegularIn < 0 ? oneTimeEvents.take() : oneTimeEvents.poll(nextRegularIn, MILLISECONDS) {
					case null:
					case event:
						eventsToRun[eventsToRunIdx++] = event;
				}
			}

			//run events
			for(i in 0...eventsToRunIdx) {
				eventsToRun[i]();
				eventsToRun[i] = null;
			}

			// Stop the loop if no regular events are active and no one-time events were executed.
			if(nextRegularIn < 0 && eventsToRunIdx == 0) {
				break;
			}
			// No one-time events were executed, but there's time to wait for a regular event
			if(nextRegularIn > 0 && eventsToRunIdx == 0) {
				JavaThread.sleep(nextRegularIn);
			}
		}
	}
}

private class NativeHaxeThread extends java.lang.Thread {
	public final haxeThread:HaxeThread;

	public function new(haxeThread:HaxeThread, job:()->Void) {
		super(new Job(job));
		this.haxeThread = haxeThread;
	}

	override overload public function run() {
		super.run();
		haxeThread.processEvents();
	}
}

private class RegularEvent {
	public var nextRunTime:Long;
	public final interval:Int;
	public final run:()->Void;
	public var next:Null<RegularEvent>;
	public var previous:Null<RegularEvent>;

	public function new(run:()->Void, nextRunTime:Long, interval:Int) {
		this.run = run;
		this.nextRunTime = nextRunTime;
		this.interval = interval;
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