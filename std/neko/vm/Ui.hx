/*
 * Copyright (C)2005-2017 Haxe Foundation
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

/**
	Core native User Interface support. This API uses native WIN32 API 
	on Windows, Carbon API on OSX, and GTK2 on Linux.
*/
class Ui {

	/**
		Tells if the current thread is the main loop thread or not. 
		The main loop thread is the one in which the first "ui" 
		library primitive has been loaded.
	*/
	public static function isMainThread() {
		return _is_main_thread();
	}

	/**
		Starts the native UI event loop. This method can only be called 
		from the main thread.
	*/
	public static function loop() {
		_loop();
	}

	/**
		Stop the native UI event loop. This method can only be called 
		from the main thread.
	*/
	public static function stopLoop() {
		_sync(_stop_loop);
	}

	/**
		Queue a method call callb to be executed by the main thread while 
		running the UI event loop. This can be used to perform UI updates 
		in the UI thread using results processed by another thread.
	*/
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
