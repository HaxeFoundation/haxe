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

import python.internal.UBuiltins.list;

/**
	This type represents python `*args` feature, supporting
	passing arbitrary number of arguments to a function.

	Example:

	```haxe
	function f(args:VarArgs<Int>) {}
	f([1, 2, 3]);
	```
**/
@:analyzer(no_simplification)
abstract VarArgs<T>(Dynamic) {
	inline function new(d:Array<T>) {
		this = d;
	}

	inline function raw():Dynamic {
		return this;
	}

	@:to public inline function toArray():Array<T> {
		return if (!Std.is(raw(), Array)) list(raw()) else (raw() : Array<T>);
	}

	@:from static inline function fromArray<T>(d:Array<T>):VarArgs<T> {
		return new VarArgs(d);
	}
}
