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

package haxe.macro;

import haxe.macro.Expr;

/**
	This class provides some utility methods to work with strings in macro
	context.
**/
class MacroStringTools {
	#if macro
	/**
		Formats `String` `s` using the usual interpolation rules.

		The returned expression is a concatenation of string parts and escaped
		elements.
	**/
	static public function formatString(s:String, pos:Position):Expr {
		#if (neko || eval)
		return Context.load("format_string", 2)(s, pos);
		#end
	}

	/**
		Tells if `e` is a format string, i.e. uses single quotes `'` as
		delimiters.
	**/
	static public function isFormatExpr(e:ExprOf<String>):Bool {
		return e.expr.match(EConst(CString(_, SingleQuotes)));
	}
	#end

	/**
		Converts an array of Strings `sl` to a field expression.

		If `sl` has no elements, the result is null.

		If `sl` has one element, the result is `EConst(CIdent(sl[0])`.

		Otherwise the result is a chain of `EField` nodes.

		If `sl` is null, the result is unspecified.
	**/
	static public function toFieldExpr(sl:Array<String>, ?pos):Expr {
		if (pos == null)
			return Lambda.fold(sl, function(s, e) return e == null ? (macro $i{s}) : (macro $e.$s), null);
		var e = null;
		for (v in sl)
			if (e == null)
				e = {expr: EConst(CIdent(v)), pos: pos};
			else
				e = {expr: EField(e, v), pos: pos};
		return e;
	}

	/**
		Converts a path given by package `pack` and name `name` to a `String`
		separated by dots.

		If `pack` has no elements, the result is `name`.

		If `pack` is null, the result is unspecified.

		Otherwise the elements of `pack` are joined with a separating dot, with
		an appended dot separating the result from `name`.
	**/
	static public function toDotPath(pack:Array<String>, name:String):String {
		return if (pack.length == 0) name else pack.join(".") + "." + name;
	}

	static public function toComplex(path:String):ComplexType {
		var pack = path.split(".");
		if(pack.length >= 2 && ~/^[A-Z]/.match(pack[pack.length - 2])) {
			return TPath({pack: pack, sub: pack.pop(), name: pack.pop(), params: []});
		} else {
			return TPath({pack: pack, name: pack.pop(), params: []});
		}
	}
}
