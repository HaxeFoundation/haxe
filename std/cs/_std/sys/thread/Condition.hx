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

@:coreApi
class Condition {
	var _lock:Dynamic;

	public function new() {
		_lock = cs.Syntax.code("new object()");
	}

	public function acquire():Void {
		cs.Syntax.code("System.Threading.Monitor.Enter({0})", _lock);
	}

	public function tryAcquire():Bool {
		return cs.Syntax.code("System.Threading.Monitor.TryEnter({0})", _lock);
	}

	public function release():Void {
		cs.Syntax.code("System.Threading.Monitor.Exit({0})", _lock);
	}

	public function wait():Void {
		cs.Syntax.code("System.Threading.Monitor.Wait({0})", _lock);
	}

	public function signal():Void {
		cs.Syntax.code("System.Threading.Monitor.Pulse({0})", _lock);
	}

	public function broadcast():Void {
		cs.Syntax.code("System.Threading.Monitor.PulseAll({0})", _lock);
	}
}
