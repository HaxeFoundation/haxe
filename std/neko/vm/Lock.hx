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

class Lock {
	var l : Dynamic;

	/**
		Creates a lock which is initially locked.
	*/
	public function new() {
		l = lock_create();
	}

	/**
		Waits for a lock to be released and acquire it. If timeout 
		(in seconds) is not `null` and expires then the returned 
		value is `false`.
	*/
	public function wait( ?timeout : Float ) : Bool {
		return lock_wait(l,timeout);
	}
	
	/**
		Release a lock. The thread does not need to own the lock 
		to be able to release it. If a lock is released several 
		times, it can be acquired as many times.
	*/
	public function release() {
		lock_release(l);
	}

	static var lock_create = neko.Lib.load("std","lock_create",0);
	static var lock_release = neko.Lib.load("std","lock_release",1);
	static var lock_wait = neko.Lib.load("std","lock_wait",2);
}
