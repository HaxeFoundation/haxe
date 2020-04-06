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

package cpp;

@:scalar @:coreType
extern abstract AtomicInt from(Int) to(Int) {
	/**
		Returns true if exchange took place.
	**/
	@:native("_hx_atomic_exchange_if")
	public static function exchangeIf(ioValue:Pointer<AtomicInt>, test:Int, newVal:Int):Bool;

	/**
		Returns value before increment.
	**/
	@:native("_hx_atomic_inc")
	public static function atomicInc(ioValue:Pointer<AtomicInt>):Int;

	/**
		Returns value before decrement.
	**/
	@:native("_hx_atomic_dec")
	public static function atomicDec(ioValue:Pointer<AtomicInt>):Int;
}
