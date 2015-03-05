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
package python.lib;

import python.NativeIterator;
import python.NativeIterable;

@:pythonImport("builtins", "set")
extern class Set <T>
{
	@:overload(function (?array:Array<T>):Void {})
	public function new (?iterable:NativeIterable<T>):Void;

	public inline function length ():Int
	{
		return python.internal.UBuiltins.len(this);
	}

	public inline function has (v:T):Bool
	{
		return python.Syntax.isIn(v, this);
	}


	public inline function minus (other:Set<T>):Set<T>
	{
		return python.Syntax.binop(this, "-", other);
	}
	public inline function plus (other:Set<T>):Set<T>
	{
		return python.Syntax.binop(this, "+", other);
	}

	function __iter__ ():NativeIterator<T>;

	public inline function iterator ():NativeIterator<T>
	{
		return __iter__();
	}
}