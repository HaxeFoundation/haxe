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

import java.lang.Runnable;
import java.lang.System;
import java.lang.Thread as JavaThread;
import java.util.Collections;
import java.util.WeakHashMap;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.atomic.AtomicInteger;
import jvm.Int64 as Long;

abstract ThreadImpl(JavaThread) {

	function toNative() {
		return this;
	}

	public static function current():ThreadImpl {
		return cast JavaThread.currentThread();
	}

	public static function create(job:() -> Void):ThreadImpl {
		return cast new NativeHaxeThread(job);
	}

	public static function setName( t : ThreadImpl, name : String ) {
		return t.toNative().setName(name);
	}

	public static function getName( t : ThreadImpl ) {
		return t.toNative().getName();
	}

}

private class NativeHaxeThread extends java.lang.Thread {

	var job : Void -> Void;

	public function new(job) {
		super();
		this.job = job;
		setDaemon(true);
		start();
	}

	public overload override function run() {
		job();
	}

}

