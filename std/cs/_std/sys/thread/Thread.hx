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

abstract Thread(Dynamic) {
	public var events(get, never):EventLoop;

	inline function get_events():EventLoop {
		return null; // TODO: implement EventLoop
	}

	public static function create(job:() -> Void):Thread {
		var thread:Dynamic = untyped __cs__("new System.Threading.Thread(() => {0}())", job);
		untyped __cs__("{0}.IsBackground = true", thread);
		untyped __cs__("{0}.Start()", thread);
		return cast thread;
	}

	public static function current():Thread {
		return cast untyped __cs__("System.Threading.Thread.CurrentThread");
	}

	public static function runWithEventLoop(job:() -> Void):Void {
		// TODO: implement EventLoop
		job();
	}

	public static function readMessage(block:Bool):Dynamic {
		throw new haxe.exceptions.NotImplementedException();
	}

	@:ifFeature("has_threads")
	public function sendMessage(msg:Dynamic):Void {
		throw new haxe.exceptions.NotImplementedException();
	}
}
