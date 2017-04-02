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

package haxe.macro;

import haxe.macro.Expr;

/**
	This class provides some utility methods to work with AST-level types. It is
	best used through `using haxe.macro.ComplexTypeTools` syntax and then provides
	additional methods on `haxe.macro.ComplexType` instances.
**/
class ComplexTypeTools {

	/**
		Converts type `c` to a human-readable `String` representation.

		The result is guaranteed to be valid Haxe code, but there may be
		differences from the original lexical syntax.
	**/
	static public function toString( c : ComplexType ) : String
		return new Printer().printComplexType(c);

	#if macro

	/**
		Returns a type corresponding to `c`.

		If `c` is null, the result is null.
	**/
	static public function toType( c : ComplexType ) : Null<Type>
		return c == null ? null : Context.resolveType(c,Context.currentPos());

	#end
}
