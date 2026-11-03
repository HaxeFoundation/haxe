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

@:include("hx/thread/Thread.hpp")
@:cpp.ManagedType({ namespace : [ "hx", "thread" ], type : "Thread", flags : [ StandardNaming ] })
private extern class NativeThread {
	static function create(job:()->Void):NativeThread;
	static function current():NativeThread;

	function getName():String;
	function setName(name:String):Void;
}

abstract ThreadImpl(NativeThread) {

	public static #if !scriptable inline #end function current():ThreadImpl {
		return cast NativeThread.current();
	}

	public static #if !scriptable inline #end function create(job:Void->Void):ThreadImpl {
		return cast NativeThread.create(job);
	}

	public static function setName( t : ThreadImpl, name : String ) {
		(cast t : NativeThread).setName(name);
	}

	public static function getName( t : ThreadImpl ) {
		return (cast t : NativeThread).getName();
	}

}
