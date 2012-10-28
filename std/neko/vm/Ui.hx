/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package neko.vm;

class Ui {

	public static function isMainThread() {
		return _is_main_thread();
	}

	public static function loop() {
		_loop();
	}

	public static function stopLoop() {
		_sync(_stop_loop);
	}

	public static function sync( f : Void -> Void ) {
		_sync(f);
	}

	public static function syncResult<T>( f : Void -> T ) : T {
		if( isMainThread() )
			return f();
		var l = new Lock();
		var tmp = null;
		var exc = null;
		_sync(function() {
			try {
				tmp = f();
			} catch( e : Dynamic ) {
				exc = { v : e };
			}
			l.release();
		});
		l.wait();
		if( exc != null )
			throw exc.v;
		return tmp;
	}

	static var _is_main_thread = neko.Lib.load("ui","ui_is_main",0);
	static var _loop = neko.Lib.load("ui","ui_loop",0);
	static var _stop_loop = neko.Lib.load("ui","ui_stop_loop",0);
	static var _sync = neko.Lib.load("ui","ui_sync",1);

}