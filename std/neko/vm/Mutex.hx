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
	Creates a mutex, which can be used to acquire a temporary lock 
	to access some ressource. The main difference with a lock is 
	that a mutex must always be released by the owner thread.
*/
class Mutex {
	var m : Dynamic;

	/**
		Creates a mutex.
	*/
	public function new() {
		m = mutex_create();
	}

	/**
		The current thread acquire the mutex or wait if not available.
		The same thread can acquire several times the same mutex but 
		must release it as many times it has been acquired.
	*/
	public function acquire() {
		mutex_acquire(m);
	}

	/**
		Try to acquire the mutex, returns true if acquire or false 
		if it's already locked by another thread.
	*/
	public function tryAcquire() : Bool {
		return mutex_try(m);
	}

	/**
		Release a mutex that has been acquired by the current thread. 
		The behavior is undefined if the current thread does not own
		the mutex.
	*/
	public function release() {
		mutex_release(m);
	}

	static var mutex_create = neko.Lib.loadLazy("std","mutex_create",0);
	static var mutex_release = neko.Lib.loadLazy("std","mutex_release",1);
	static var mutex_acquire = neko.Lib.loadLazy("std","mutex_acquire",1);
	static var mutex_try = neko.Lib.loadLazy("std","mutex_try",1);
}
