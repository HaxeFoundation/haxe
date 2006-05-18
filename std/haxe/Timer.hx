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
	#if flash
		private var id : Int;
	#else js
		private static var arr = new Array<Timer>();
		private var timerId : Int;
	#else error
	#end

	public function new( time : Int ){
		#if flash
			var me = this;
			id = untyped _global.setInterval(function() { me.run(); },time);
		#else js
			var id = arr.length;
			arr[id] = this;
			timerId = untyped window.setInterval("haxe.Timer.arr["+id+"].run();",time);
		#else error
		#end
	}

	public function stop(){
		#if flash
			untyped _global.clearInterval(id);
			id = null;
		#else js
			untyped window.clearInterval(timerId);
		#else error
		#end
	}

	public function run(){
	}

	public static function delayed( f : Void -> Void, time : Int ) : Void -> Void {
		return function() {
			var t = new haxe.Timer(time);
			t.run = function() {
				t.stop();
				f();
			};
		};
	}

	public static function delayedArg<T>( f : T -> Void, time : Int ) : T -> Void {
		return function(arg) {
			var t = new haxe.Timer(time);
			t.run = function() {
				t.stop();
				f(arg);
			};
		};
	}

}
