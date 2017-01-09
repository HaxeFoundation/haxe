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
package python;

import python.Dict;

/**
	This type represents python `**kwargs` feature, supporting
	passing named arguments to a function.

	Example:

	```haxe
	function f(kwargs:KwArgs<{a:Int}>) {}
	f({a: 10});
	```
**/
abstract KwArgs<T:{}>(Dict<String,Dynamic>) {
	inline function new (d:Dict<String,Dynamic>) {
		this = d;
	}

	@:to public inline function toDict():Dict<String,Dynamic> {
		// pass null, it's just to have the type information available in genpy
		return toDictHelper(null);
	}
	// this is a helper method (hack) which is matched in genpy to extract the type information for reverse mapping
	// of keyword fields.
	function toDictHelper(x:T):Dict<String,Dynamic> {
		return this;
	}

	@:from static inline function fromDict(d:Dict<String,Dynamic>):KwArgs<Dynamic> {
		return new KwArgs(d);
	}

	@:from static function fromT<T:{}>(d:T):KwArgs<T> {
		return new KwArgs(Lib.anonAsDict(d));
	}

	public inline function typed():T {
		return Lib.dictAsAnon(toDict());
	}

	public inline function get<V>(key:String, def:V):V {
		return this.get(key, def);
	}
}
