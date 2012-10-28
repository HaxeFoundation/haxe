/*
 * Copyright (C)2005-2012 Haxe Foundation
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

class Deque<T> {
	var q : Void;
	public function new() {
		q = deque_create();
	}
	public function add( i : T ) {
		deque_add(q,i);
	}
	public function push( i : T ) {
		deque_push(q,i);
	}
	public function pop( block : Bool ) : T {
		return deque_pop(q,block);
	}
	static var deque_create = neko.Lib.loadLazy("std","deque_create",0);
	static var deque_add = neko.Lib.loadLazy("std","deque_add",2);
	static var deque_push = neko.Lib.loadLazy("std","deque_push",2);
	static var deque_pop = neko.Lib.loadLazy("std","deque_pop",2);
}
