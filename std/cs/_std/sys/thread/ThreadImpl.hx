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

abstract ThreadImpl(cs.system.threading.Thread) {
	inline function toNative():cs.system.threading.Thread {
		return this;
	}

	public static inline function current():ThreadImpl {
		return cast cs.system.threading.Thread.CurrentThread;
	}

	public static function create(job:() -> Void):ThreadImpl {
		// Wrap the haxe.lang.Function in a C# lambda that calls invoke()
		// This is necessary because haxe.lang.Function is not a C# delegate
		var start:cs.system.threading.ThreadStart = cs.Syntax.code("new global::System.Threading.ThreadStart(() => {0}.invoke())", job);
		var thread = new cs.system.threading.Thread(start);
		thread.IsBackground = true;
		thread.Start();
		return cast thread;
	}

	public static inline function setName(t:ThreadImpl, name:String):Void {
		t.toNative().Name = name;
	}

	public static inline function getName(t:ThreadImpl):Null<String> {
		return t.toNative().Name;
	}
}
