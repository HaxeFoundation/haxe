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

import haxe.macro.Type;

/**
	This class provides some utility methods to work with typed expressions.
	It is best used through 'using haxe.macro.TypedExprTools' syntax and then
	provides additional methods on `haxe.macro.TypedExpr` instances.
**/
class TypedExprTools {
	static function with(e:TypedExpr, ?edef:TypedExprDef, ?t:Type) {
		return {
			expr: edef == null ? e.expr : edef,
			pos: e.pos,
			t: t == null ? e.t : t
		}
	}

	/**
		Transforms the sub-expressions of `e` by calling `f` on each of them.

		See `haxe.macro.ExprTools.map` for details on expression mapping in
		general. This function works the same way, but with a different data
		structure.
	**/
	static public function map(e:TypedExpr, f:TypedExpr -> TypedExpr):TypedExpr {
		return switch(e.expr) {
			case TConst(_) | TLocal(_) | TBreak | TContinue | TTypeExpr(_): e;
			case TArray(e1, e2): with(e, TArray(f(e1), f(e2)));
			case TBinop(op, e1, e2): with(e, TBinop(op, f(e1), f(e2)));
			case TFor(v, e1, e2): with(e, TFor(v, f(e1), f(e2)));
			case TWhile(e1, e2, flag): with(e, TWhile(f(e1), f(e2), flag));
			case TThrow(e1): with(e, TThrow(f(e1)));
			case TEnumParameter(e1, ef, i): with(e, TEnumParameter(f(e1), ef, i));
			case TField(e1, fa): with(e, TField(f(e1), fa));
			case TParenthesis(e1): with(e, TParenthesis(f(e1)));
			case TUnop(op, pre, e1): with(e, TUnop(op, pre, f(e1)));
			case TArrayDecl(el): with(e, TArrayDecl(el.map(f)));
			case TNew(t, pl, el): with(e, TNew(t, pl, el.map(f)));
			case TBlock(el): with(e, TBlock(el.map(f)));
			case TObjectDecl(fl): with(e, TObjectDecl(fl.map(function(field) return { name: field.name, expr: f(field.expr) })));
			case TCall(e1, el): with(e, TCall(f(e1), el.map(f)));
			case TVar(v,eo): with(e, TVar(v, eo == null ? null : f(eo)));
			case TFunction(fu): with(e, TFunction({ t: fu.t, args: fu.args, expr: f(fu.expr)}));
			case TIf(e1, e2, e3): with(e, TIf(f(e1), f(e2), e3 == null ? null : f(e3)));
			case TSwitch(e1, cases, e2): with(e, TSwitch(f(e1), cases.map(function(c) return { values: c.values.map(f), expr: f(c.expr) }), e2 == null ? null : f(e2)));
			case TTry(e1, catches): with(e, TTry(f(e1), catches.map(function(c) return { v:c.v, expr: f(c.expr) })));
			case TReturn(e1): with(e, TReturn(e1 == null ? null : f(e1)));
			case TCast(e1, mt): with(e, TCast(f(e1), mt));
			case TMeta(m, e1): with(e, TMeta(m, f(e1)));
		}
	}

	/**
		Calls function `f` on each sub-expression of `e`.

		See `haxe.macro.ExprTools.iter` for details on iterating expressions in
		general. This function works the same way, but with a different data
		structure.
	**/
	static public function iter(e:TypedExpr, f:TypedExpr -> Void):Void {
		switch(e.expr) {
			case TConst(_) | TLocal(_) | TBreak | TContinue | TTypeExpr(_):
			case TArray(e1, e2) | TBinop(_, e1, e2) | TFor(_, e1, e2) | TWhile(e1, e2, _):
				f(e1);
				f(e2);
			case TThrow(e1) | TEnumParameter(e1, _, _) | TField(e1, _) | TParenthesis(e1) | TUnop(_, _, e1) | TCast(e1, _) | TMeta(_, e1):
				f(e1);
			case TArrayDecl(el) | TNew(_, _, el) | TBlock(el):
				for (e in el) f(e);
			case TObjectDecl(fl):
				for (field in fl) f(field.expr);
			case TCall(e1, el):
				f(e1);
				for (e in el) f(e);
			case TVar(_, e1) | TReturn(e1):
				if (e1 != null) f(e1);
			case TFunction(fu):
				f(fu.expr);
			case TIf(e1, e2, e3):
				f(e1);
				f(e2);
				if (e3 != null) f(e3);
			case TSwitch(e1, cases, e2):
				f(e1);
				for (c in cases) {
					for (v in c.values) f(v);
					f(c.expr);
				}
				if (e2 != null) f(e2);
			case TTry(e1, catches):
				f(e1);
				for (c in catches) f(c.expr);
		}
	}

	/**
		Transforms the sub-expressions of `e` by calling `f` on each of them.
		Additionally, types are mapped using `ft` and variables are mapped using
		`fv`.

		See `haxe.macro.ExprTools.map` for details on expression mapping in
		general. This function works the same way, but with a different data
		structure.
	**/
	static public function mapWithType(e:TypedExpr, f:TypedExpr -> TypedExpr, ft:Type -> Type, fv:TVar -> TVar):TypedExpr {
		return switch(e.expr) {
			case TConst(_) | TBreak | TContinue | TTypeExpr(_): with(e, ft(e.t));
			case TLocal(v): with(e, TLocal(fv(v)), ft(e.t));
			case TArray(e1, e2): with(e, TArray(f(e1), f(e2)), ft(e.t));
			case TBinop(op, e1, e2): with(e, TBinop(op, f(e1), f(e2)), ft(e.t));
			case TFor(v, e1, e2): with(e, TFor(fv(v), f(e1), f(e2)), ft(e.t));
			case TWhile(e1, e2, flag): with(e, TWhile(f(e1), f(e2), flag), ft(e.t));
			case TThrow(e1): with(e, TThrow(f(e1)), ft(e.t));
			case TEnumParameter(e1, ef, i): with(e, TEnumParameter(f(e1), ef, i), ft(e.t));
			case TField(e1, fa): with(e, TField(f(e1), fa), ft(e.t));
			case TParenthesis(e1): with(e, TParenthesis(e1), ft(e.t));
			case TUnop(op, pre, e1): with(e, TUnop(op, pre, f(e1)), ft(e.t));
			case TArrayDecl(el): with(e, TArrayDecl(el.map(f)), ft(e.t));
			case TNew(t, pl, el): with(e, TNew(t, pl, el.map(f)), ft(e.t));
			case TBlock(el): with(e, TBlock(el.map(f)), ft(e.t));
			case TObjectDecl(fl): with(e, TObjectDecl(fl.map(function(field) return { name: field.name, expr: f(field.expr) })), ft(e.t));
			case TCall(e1, el): with(e, TCall(f(e1), el.map(f)), ft(e.t));
			case TVar(v,eo): with(e, TVar(fv(v), eo == null ? null : f(eo)), ft(e.t));
			case TFunction(fu): with(e, TFunction({ t: ft(fu.t), args: fu.args.map(function(arg) return { v: fv(arg.v), value: arg.value }), expr: f(fu.expr)}), ft(e.t));
			case TIf(e1, e2, e3): with(e, TIf(f(e1), f(e2), e3 == null ? null : f(e3)), ft(e.t));
			case TSwitch(e1, cases, e2): with(e, TSwitch(f(e1), cases.map(function(c) return { values: c.values.map(f), expr: f(c.expr) }), e2 == null ? null : f(e2)), ft(e.t));
			case TTry(e1, catches): with(e, TTry(f(e1), catches.map(function(c) return { v:fv(c.v), expr: f(c.expr) })), ft(e.t));
			case TReturn(e1): with(e, TReturn(e1 == null ? null : f(e1)), ft(e.t));
			case TCast(e1, mt): with(e, TCast(f(e1), mt), ft(e.t));
			case TMeta(m, e1): with(e, TMeta(m, f(e1)), ft(e.t));
		}
	}

	#if macro
	static public function toString(t:TypedExpr, ?pretty = false):String {
		return @:privateAccess haxe.macro.Context.sExpr(t, pretty);
	}
	#end
}
