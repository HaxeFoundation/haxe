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
	A message queue for multithread access.
*/
class Deque<T> {
	var q : Dynamic;

	/**
		Create a message queue for multithread access.
	*/
	public function new() {
		q = deque_create();
	}

	/**
		Add a message at the end of the queue.
	*/
	public function add( i : T ) {
		deque_add(q,i);
	}

	/**
		Add a message at the head of the queue.
	*/
	public function push( i : T ) {
		deque_push(q,i);
	}

	/**
		Pop a message from the queue head. Either block until a message 
		is available or return immediately with `null`.
	*/
	public function pop( block : Bool ) : Null<T> {
		return deque_pop(q,block);
	}

	static var deque_create = neko.Lib.loadLazy("std","deque_create",0);
	static var deque_add = neko.Lib.loadLazy("std","deque_add",2);
	static var deque_push = neko.Lib.loadLazy("std","deque_push",2);
	static var deque_pop = neko.Lib.loadLazy("std","deque_pop",2);
}
