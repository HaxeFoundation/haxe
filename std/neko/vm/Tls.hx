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
	Creates thread local storage.
*/
class Tls<T> {

	var t : Dynamic;
	public var value(get,set) : T;

	/**
		Creates thread local storage. This is placeholder that can store
		a value that will be different depending on the local thread. 
		Set the tls value to `null` before exiting the thread 
		or the memory will never be collected.
	*/
	public function new() {
		t = tls_create();
	}

	/**
		Returns the value set by tls_set for the local thread.
	*/
	function get_value() : T {
		return tls_get(t);
	}

	/**
		Set the value of the TLS for the local thread.
	*/
	function set_value( v : T ) {
		tls_set(t,v);
		return v;
	}

	static var tls_create = neko.Lib.load("std","tls_create",0);
	static var tls_get = neko.Lib.load("std","tls_get",1);
	static var tls_set = neko.Lib.load("std","tls_set",2);
}
