package haxe.macro;
import haxe.macro.Expr;

/**
	This class provides some utility methods to work with expressions. It is
	best used through 'using haxe.macro.exprTools' syntax and then provides
	additional methods on haxe.macro.Expr instances.
	
	While mainly intended to be used in macros, it works in non-macro code as
	well.
**/
class ExprTools {
	
	/**
		Converts expression [e] to a human-readable String representation.
		
		The result is guaranteed to be valid haxe code, but there may be
		differences from the original lexical syntax.
	**/
	static public function toString( e : Expr ) : String
		return new Printer().printExpr(e)
		
	/**
		Calls function [f] on each sub-expression of [e].
		
		If [e] has no sub-expressions, this operation has no effect.
	
		Otherwise [f] is called once per sub-expression of [e], with the
		sub-expression as argument. These calls are done in order of the
		sub-expression declarations.
		
		This method does not call itself recursively. It should instead be used
		in a recursive function which handles the expression nodes of interest.
		
		Usage example:
			
		function findStrings(e:Expr) {
			switch(e.expr) {
				case EConst(CString(s)):
					// handle s
				case _:
					ExprTools.iter(e, findStrings);
			}
		}
	**/
	static public function iter( e : Expr, f : Expr -> Void ) : Void {
		switch(e.expr) {
			case EConst(_),
				EContinue,
				EBreak,
				EDisplayNew:
			case EField(e, _),
				EParenthesis(e),
				EUntyped(e),
				EThrow(e),
				EDisplay(e, _),
				ECheckType(e, _),
				EUnop(_, _, e),
				ECast(e, _),
				#if !haxe3
				EType(e, _),
				#end
				EMeta(_, e):
					f(e);
			case EArray(e1, e2),
				EWhile(e1, e2, _),
				EBinop(_, e1, e2),
				EFor(e1, e2),
				EIn(e1, e2):
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
		}
	}
	
	/**
		Transforms the sub-expressions of [e] by calling [f] on each of them.
		
		If [e] has no sub-expressions, this operation returns [e] unchanged.
	
		Otherwise [f] is called once per sub-expression of [e], with the
		sub-expression as argument. These calls are done in order of the
		sub-expression declarations.
		
		This method does not call itself recursively. It should instead be used
		in a recursive function which handles the expression nodes of interest.
		
		Usage example:
		
		function capitalizeStrings(e:Expr) {
			return switch(e.expr) {
				case EConst(CString(s)):
					{ expr: EConst(CString(s.toUpperCase())), pos: e.pos };
				case _:
					ExprTools.map(e, capitalizeStrings);
			}
		}
	**/
	static public function map( e : Expr, f : Expr -> Expr ) : Expr {
		return {pos: e.pos, expr: switch(e.expr) {
			case EConst(_): e.expr;
			case EArray(e1, e2): EArray(f(e1), f(e2));
			case EBinop(op, e1, e2): EBinop(op, f(e1), f(e2));
			case EField(e, field): EField(f(e), field);
			case EParenthesis(e): EParenthesis(f(e));
			case EObjectDecl(fields):
				var ret = [];
				for (field in fields)
					ret.push( { field: field.field, expr: f(field.expr) } );
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
			case EIn(e1, e2): EIn(f(e1), f(e2));
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
				ESwitch(f(e), ret, opt(edef, f));
			case EFunction(name, func):
				var ret = [];
				for (arg in func.args)
					ret.push( { name: arg.name, opt: arg.opt, type: arg.type, value: opt(arg.value, f) } );
				EFunction(name, { args: ret, ret: func.ret, params: func.params, expr: f(func.expr) } );
			#if !haxe3
			case EType(e, field): EType(f(e), field);
			#end
			case EMeta(m, e): EMeta(m, f(e));
		}};
	}
	
	static inline function opt(e:Null<Expr>, f : Expr -> Expr):Expr
		return e == null ? null : f(e)
		
	static inline function opt2(e:Null<Expr>, f : Expr -> Void):Void
		if (e != null) f(e)		
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