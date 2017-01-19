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

import python.HaxeIterable;
import python.NativeIterator;

/**
    This type represents native Python iterables (objects that implement `__iter__()` method).
    It supports Haxe iteration and conversion to `Iterable` by creating wrapper objects.
**/
abstract NativeIterable<T>(NativeIterableRaw<T>) to NativeIterableRaw<T> from NativeIterableRaw<T> {

    /**
        Return Haxe `Iterable` object wrapping `this` native iterable.
    **/
	@:to public inline function toHaxeIterable():HaxeIterable<T> return new HaxeIterable(this);

    /**
        Return Haxe `Iterator` object by wrapping `this.__iter__()` native iterator.
    **/
	public inline function iterator():HaxeIterator<T> return new HaxeIterator(this.__iter__());
}

/**
    Native Python iterable protocol.
**/
typedef NativeIterableRaw<T> = {
	private function __iter__():NativeIterator<T>;
}
