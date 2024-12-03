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

#if (!target.threaded)
#error "This class is not available on this target"
#end

/**
	Creates a mutex, which can be used to acquire a temporary lock
	to access some resource. The main difference with a lock is
	that a mutex must always be released by the owner thread.
**/
extern class Mutex {
	/**
		Creates a mutex.
	**/
	function new():Void;

	/**
		The current thread acquire the mutex or wait if not available.
		The same thread can acquire several times the same mutex but
		must release it as many times it has been acquired.
	**/
	function acquire():Void;

	/**
		Try to acquire the mutex, returns true if acquire or false
		if it's already locked by another thread.
	**/
	function tryAcquire():Bool;

	/**
		Release a mutex that has been acquired by the current thread.
		The behavior is undefined if the current thread does not own
		the mutex.
	**/
	function release():Void;
}
