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
package cs;

/**
	This type represents pointer types for C# function parameters. It should only
	be used inside an unsafe context (not checked by the Haxe compiler)

	C# code:
		int[] src;
		fixed (int* pSrc = src)
		{
			...
		}
	Haxe code:
		var pSrc:cs.Pointer<Int>;
		cs.Lib.fixed(pSrc = cast src,
		{
			...
		});

**/
#if !unsafe
#error "You need to define 'unsafe' to be able to use unsafe code in hxcs"
#else
extern class Pointer<T> /*extends Int,*/ implements ArrayAccess<T>
{
	static function op_Addition<T>(p:Pointer<T>, i:Int):Pointer<T>;
	public inline function add(i:Int):Pointer<T>
	{
		return op_Addition(this,i);
	}
}
#end
