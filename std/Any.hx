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

/**
	`Any` is a type that is compatible with any other in both ways.

	This means that a value of any type can be assigned to `Any`, and
	vice-versa, a value of `Any` type can be assigned to any other type.

	It's a more type-safe alternative to `Dynamic`, because it doesn't
	support field access or operators and it's bound to monomorphs. So,
	to work with the actual value, it needs to be explicitly promoted
	to another type.
**/
abstract Any(Dynamic) {
	@:noCompletion @:extern @:to inline function __promote<T>():T return this;
	@:noCompletion @:extern @:from inline static function __cast<T>(value:T):Any return cast value;
	@:noCompletion @:extern inline function toString():String return Std.string(this);
}
