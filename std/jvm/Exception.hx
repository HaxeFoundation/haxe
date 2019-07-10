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

package jvm;

@:keep
@:native('haxe.jvm.Exception')
class Exception<T> extends java.lang.Exception {
	static public var exception = new java.lang.ThreadLocal<java.lang.Throwable>();

	static public function setException(exc:java.lang.Throwable) {
		exception.set(exc);
	}

	static public function currentException() {
		return exception.get();
	}

	public var value:T;

	public function new(value:T) {
		super();
		this.value = value;
	}

	@:overload override public function toString() {
		return Std.string(value);
	}

	public function unwrap() {
		return value;
	}

	static public function wrap<T>(t:Null<T>) {
		if (Jvm.instanceof(t, java.lang.Exception)) {
			return (cast t : java.lang.Exception);
		} else {
			return new Exception(t);
		}
	}
}
