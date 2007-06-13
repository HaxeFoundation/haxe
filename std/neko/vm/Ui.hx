/*
 * Copyright (c) 2005-2007, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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