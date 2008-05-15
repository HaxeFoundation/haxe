/*
 * Copyright (c) 2005, The haXe Project Contributors
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
package haxe;

class Timer {

	private var id : Null<Int>;

	#if js
	private static var arr = new Array<Timer>();
	private var timerId : Int;
	#end

	#if !neko

	public function new( time : Int ){
		#if flash9
			var me = this;
			id = untyped __global__["flash.utils.setInterval"](function() { me.run(); },time);
		#elseif flash
			var me = this;
			id = untyped _global["setInterval"](function() { me.run(); },time);
		#elseif js
			id = arr.length;
			arr[id] = this;
			timerId = untyped window.setInterval("haxe.Timer.arr["+id+"].run();",time);
		#end
	}

	public function stop() {
		if( id == null )
			return;
		#if flash9
			untyped __global__["flash.utils.clearInterval"](id);
		#elseif flash
			untyped _global["clearInterval"](id);
		#elseif js
			untyped window.clearInterval(timerId);
			arr[id] = null;
			if( id > 100 && id == arr.length - 1 ) {
				// compact array
				var p = id - 1;
				while( p >= 0 && arr[p] == null )
					p--;
				arr = arr.slice(0,p+1);
			}
		#end
		id = null;
	}

	public dynamic function run() {
	}

	public static function delay( f : Void -> Void, time : Int ) {
		var t = new haxe.Timer(time);
		t.run = function() {
			t.stop();
			f();
		};
	}

	#end

	/**
		Returns a timestamp, in seconds
	**/
	public static function stamp() : Float {
		#if flash
			return flash.Lib.getTimer() / 1000;
		#elseif neko
			return neko.Sys.time();
		#elseif js
			return Date.now().getTime() / 1000;
		#else
			return 0;
		#end
	}

}
