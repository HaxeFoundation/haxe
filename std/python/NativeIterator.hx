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

import python.NativeIterable.NativeIterableRaw;

/**
    This type represents native Python iterators.
    It supports automatic conversion to Haxe `Iterator` by creating wrapper object.
**/
abstract NativeIterator<T>(NativeIteratorRaw<T>) to NativeIteratorRaw<T> to NativeIterable<T> {
	public inline function new(p:NativeIteratorRaw<T>) this = p;

    /**
        Return Haxe `Iterator` object by wrapping `this` native iterator.
    **/
	@:to public inline function toHaxeIterator():HaxeIterator<T> return new HaxeIterator(this);
}

/**
    Native Python iterator protocol.
**/
typedef NativeIteratorRaw<T> = {
	>NativeIterableRaw<T>,
	function __next__():T;
}
