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
	A Deque is a double-ended queue with a `pop` method that can block until
	an element is available. It is commonly used to synchronize threads.
 */
@:coreApi extern class Deque<T> {
	/**
		Create a new Deque instance which is initially empty.
	**/
	function new():Void;

	/**
		Adds an element at the end of `this` Deque.

		(Java,Jvm): throws `java.lang.NullPointerException` if `i` is `null`.
	**/
	function add(i:T):Void;

	/**
		Adds an element at the front of `this` Deque.

		(Java,Jvm): throws `java.lang.NullPointerException` if `i` is `null`.
	**/
	function push(i:T):Void;

	/**
		Tries to retrieve an element from the front of `this` Deque.

		If an element is available, it is removed from the queue and returned.

		If no element is available and `block` is `false`, `null` is returned.

		Otherwise, execution blocks until an element is available and returns it.
	**/
	function pop(block:Bool):Null<T>;
}
