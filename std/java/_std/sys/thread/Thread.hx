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

abstract Thread(ThreadHandle) {
	inline function new(t:ThreadHandle)
	{
		this = t;
	}

	public static function create(callb:Void->Void):Thread
	{
		var ret = new ThreadHandle();
		var t = new HaxeThread(ret, callb);
		t.start();
		return new Thread(ret);
	}

	public static function current():Thread
	{
		return new Thread(ThreadHandle.getThread( java.lang.Thread.currentThread() ));
	}

	public static function readMessage(block : Bool) : Dynamic
	{
		return current().getHandle().messages.pop(block);
	}

	public inline function sendMessage(msg:Dynamic):Void
	{
		this.sendMessage(msg);
	}

	private inline function getHandle():ThreadHandle {
		return this;
	}
}

@:native('haxe.java.vm.Thread') private class ThreadHandle
{
	@:private static var javaThreadToHaxe = new haxe.ds.WeakMap<java.lang.Thread, ThreadHandle>();
	@:private static var mainJavaThread = java.lang.Thread.currentThread();
	@:private static var mainHaxeThread = {
		var ret = new ThreadHandle();
		javaThreadToHaxe.set(mainJavaThread, ret);
		ret;
	};

	public static function getThread(jt:java.lang.Thread):ThreadHandle
	{
		if (Std.is(jt, HaxeThread))
		{
			var t:HaxeThread = cast jt;
			return t.threadObject;
		}
		else if (jt == mainJavaThread)
		{
			return mainHaxeThread;
		}
		else
		{
			var ret = null;
			untyped __lock__(javaThreadToHaxe, {
				ret = javaThreadToHaxe.get(jt);
				if (ret == null)
				{
					ret = new ThreadHandle();
					javaThreadToHaxe.set(jt, ret);
				}

			});
			return ret;
		}
	}

	public var messages:Deque<Dynamic>;

	public function new()
	{
		this.messages = new Deque();
	}

	public function sendMessage(msg:Dynamic):Void
	{
		messages.add(msg);
	}
}

@:native('haxe.java.vm.HaxeThread')
private class HaxeThread extends java.lang.Thread
{
	public var threadObject(default, null):ThreadHandle;
	private var runFunction:Void->Void;
	@:overload override public function run():Void
	{
		runFunction();
	}
	public function new(hxThread:ThreadHandle, run:Void->Void)
	{
		super();
		threadObject = hxThread;
		runFunction = run;
		setDaemon(true);
	}
}
