/*
 * Copyright (C)2005-2018 Haxe Foundation
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
using Lambda;

/**
	This class provides some utility methods to work with expressions. It is
	best used through 'using haxe.macro.ExprTools' syntax and then provides
	additional methods on haxe.macro.Expr instances.

	While mainly intended to be used in macros, it works in non-macro code as
	well.
**/
class ExprTools {

	/**
		Converts expression `e` to a human-readable String representation.

		The result is guaranteed to be valid Haxe code, but there may be
		differences from the original lexical syntax.
	**/
	static public function toString( e : Expr ) : String
		return new Printer().printExpr(e);

	/**
		Calls function `f` on each sub-expression of `e`.

		If `e` has no sub-expressions, this operation has no effect.

		Otherwise `f` is called once per sub-expression of `e`, with the
		sub-expression as argument. These calls are done in order of the
		sub-expression declarations.

		This method does not call itself recursively. It should instead be used
		in a recursive function which handles the expression nodes of interest.

		Usage example:
		```haxe
		function findStrings(e:Expr) {
			switch(e.expr) {
				case EConst(CString(s)):
					// handle s
				case _:
					ExprTools.iter(e, findStrings);
			}
		}
		```
	**/
	static public function iter( e : Expr, f : Expr -> Void ) : Void {
		switch(e.expr) {
			case EConst(_),
				EContinue,
				EBreak,
				EDisplayNew(_),
				EComplexType(_):
			case EField(e, _),
				EParenthesis(e),
				EUntyped(e),
				EThrow(e),
				EDisplay(e, _),
				ECheckType(e, _),
				EUnop(_, _, e),
				ECast(e, _),
				EMeta(_, e):
					f(e);
			case EArray(e1, e2),
				EWhile(e1, e2, _),
				EBinop(_, e1, e2),
				EFor(e1, e2):
					f(e1);
					f(e2);
			case EVars(vl):
				for (v in vl)
					opt2(v.expr, f);
			case ETry(e, cl):
				f(e);
				for (c in cl)
					f(c.expr);
			case ETernary(e1, e2, e3)
			| EIf(e1, e2, e3):
				f(e1);
				f(e2);
				opt2(e3, f);
			case EArrayDecl(el),
				ENew(_, el),
				EBlock(el):
					ExprArrayTools.iter(el, f);
			case EObjectDecl(fl):
				for (fd in fl)
					f(fd.expr);
			case ECall(e, el):
				f(e);
				ExprArrayTools.iter(el, f);
			case EReturn(e):
				opt2(e, f);
			case EFunction(_, func):
				for (arg in func.args)
					opt2(arg.value, f);
				opt2(func.expr, f);
			case ESwitch(e, cl, edef):
				f(e);
				for (c in cl) {
					ExprArrayTools.iter(c.values, f);
					opt2(c.guard, f);
					opt2(c.expr, f);
				}
				if (edef != null && edef.expr != null)
					f(edef);
		}
	}

	/**
		Transforms the sub-expressions of `e` by calling `f` on each of them.

		If `e` has no sub-expressions, this operation returns `e` unchanged.

		Otherwise `f` is called once per sub-expression of `e`, with the
		sub-expression as argument. These calls are done in order of the
		sub-expression declarations.

		This method does not call itself recursively. It should instead be used
		in a recursive function which handles the expression nodes of interest.

		Usage example:
		```haxe
		function capitalizeStrings(e:Expr) {
			return switch(e.expr) {
				case EConst(CString(s)):
					{ expr: EConst(CString(s.toUpperCase())), pos: e.pos };
				case _:
					ExprTools.map(e, capitalizeStrings);
			}
		}
		```haxe
	**/
	static public function map( e : Expr, f : Expr -> Expr ) : Expr {
		return {pos: e.pos, expr: switch(e.expr) {
			case EConst(_), EComplexType(_): e.expr;
			case EArray(e1, e2): EArray(f(e1), f(e2));
			case EBinop(op, e1, e2): EBinop(op, f(e1), f(e2));
			case EField(e, field): EField(f(e), field);
			case EParenthesis(e): EParenthesis(f(e));
			case EObjectDecl(fields):
				var ret = [];
				for (field in fields)
					ret.push( { field: field.field, expr: f(field.expr), quotes: field.quotes } );
				EObjectDecl(ret);
			case EArrayDecl(el): EArrayDecl(ExprArrayTools.map(el, f));
			case ECall(e, params): ECall(f(e), ExprArrayTools.map(params, f));
			case ENew(tp, params): ENew(tp, ExprArrayTools.map(params, f));
			case EUnop(op, postFix, e): EUnop(op, postFix, f(e));
			case EVars(vars):
				var ret = [];
				for (v in vars)
					ret.push( { name: v.name, type:v.type, expr: opt(v.expr, f) } );
				EVars(ret);
			case EBlock(el): EBlock(ExprArrayTools.map(el, f));
			case EFor(it, expr): EFor(f(it), f(expr));
			case EIf(econd, eif, eelse): EIf(f(econd), f(eif), opt(eelse, f));
			case EWhile(econd, e, normalWhile): EWhile(f(econd), f(e), normalWhile);
			case EReturn(e): EReturn(opt(e,f));
			case EUntyped(e): EUntyped(f(e));
			case EThrow(e): EThrow(f(e));
			case ECast(e, t): ECast(f(e), t);
			case EDisplay(e, isCall): EDisplay(f(e), isCall);
			case ETernary(econd, eif, eelse): ETernary(f(econd), f(eif), f(eelse));
			case ECheckType(e, t): ECheckType(f(e), t);
			case EDisplayNew(_),
				EContinue,
				EBreak:
					e.expr;
			case ETry(e, catches):
				var ret = [];
				for (c in catches)
					ret.push( { name:c.name, type:c.type, expr:f(c.expr) } );
				ETry(f(e), ret);
			case ESwitch(e, cases, edef):
				var ret = [];
				for (c in cases)
					ret.push( { expr: opt (c.expr, f), guard: opt(c.guard, f), values: ExprArrayTools.map(c.values, f) } );
				ESwitch(f(e), ret, edef == null || edef.expr == null ? edef : f(edef));
			case EFunction(name, func):
				var ret = [];
				for (arg in func.args)
					ret.push( { name: arg.name, opt: arg.opt, type: arg.type, value: opt(arg.value, f) } );
				EFunction(name, { args: ret, ret: func.ret, params: func.params, expr: f(func.expr) } );
			case EMeta(m, e): EMeta(m, f(e));
		}};
	}

	/**
		Returns the value `e` represents.

		Supported expressions are:

		 - `Int`, `Float` and `String` literals
		 - identifiers `true`, `false` and `null`
		 - structure declarations if all their fields are values
		 - array declarations if all their elements are values
		 - unary operators `-`, `!` and `~` if the operand is a value
		 - binary operators except `=>`, `...` and assignments

		Parentheses, metadata and the `untyped` keyword are ignored.

		If any non-value is encountered, an exception of type `String` is
		thrown.

		If `e` is null, the result is unspecified.
	**/
	static public function getValue(e:Expr):Dynamic {
		return switch (e.expr) {
			case EConst(CInt(v)): Std.parseInt(v);
			case EConst(CFloat(v)): Std.parseFloat(v);
			case EConst(CString(s)): s;
			case EConst(CIdent("true")): true;
			case EConst(CIdent("false")): false;
			case EConst(CIdent("null")): null;
			case EParenthesis(e1) | EUntyped(e1) | EMeta(_, e1): getValue(e1);
			case EObjectDecl(fields):
				var obj = {};
				for (field in fields) {
					Reflect.setField(obj, field.field, getValue(field.expr));
				}
				obj;
			case EArrayDecl(el): el.map(getValue);
			case EIf(econd, eif, eelse) | ETernary(econd, eif, eelse):
				if (eelse == null) {
					throw "If statements only have a value if the else clause is defined";
				} else {
					var econd:Dynamic = getValue(econd);
					econd ? getValue(eif) : getValue(eelse);
				}
			case EUnop(op, false, e1):
				var e1:Dynamic = getValue(e1);
				switch (op) {
					case OpNot: !e1;
					case OpNeg: -e1;
					case OpNegBits: ~e1;
					case _: throw 'Unsupported expression: $e';
				}
			case EBinop(op, e1, e2):
				var e1:Dynamic = getValue(e1);
				var e2:Dynamic = getValue(e2);
				switch (op) {
					case OpAdd: e1 + e2;
					case OpSub: e1 - e2;
					case OpMult: e1 * e2;
					case OpDiv: e1 / e2;
					case OpMod: e1 % e2;
					case OpEq: e1 == e2;
					case OpNotEq: e1 != e2;
					case OpLt: e1 < e2;
					case OpLte: e1 <= e2;
					case OpGt: e1 > e2;
					case OpGte: e1 >= e2;
					case OpOr: e1 | e2;
					case OpAnd: e1 & e2;
					case OpXor: e1 ^ e2;
					case OpBoolAnd: e1 && e2;
					case OpBoolOr: e1 || e2;
					case OpShl: e1 << e2;
					case OpShr: e1 >> e2;
					case OpUShr: e1 >>> e2;
					case _: throw 'Unsupported expression: $e';
				}
			case _: throw 'Unsupported expression: $e';
		}
	}

	static inline function opt(e:Null<Expr>, f : Expr -> Expr):Expr
		return e == null ? null : f(e);

	static inline function opt2(e:Null<Expr>, f : Expr -> Void):Void
		if (e != null) f(e);
}

/**
	This class provides functions on expression arrays for convenience. For a
	detailed reference on each method, see the documentation of ExprTools.
 */
class ExprArrayTools {
	static public function map( el : Array<Expr>, f : Expr -> Expr):Array<Expr> {
		var ret = [];
		for (e in el)
			ret.push(f(e));
		return ret;
	}

	static public function iter( el : Array<Expr>, f : Expr -> Void):Void {
		for (e in el)
			f(e);
	}
}
