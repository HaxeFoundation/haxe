package unit.issues;

import haxe.macro.Expr;
import haxe.macro.ExprTools;
import haxe.macro.Printer;

class Issue6531 extends unit.Test {
	function test() {
		peq(macro a in b in c, "(a in (b in c))");
		peq(macro a % b in c, "(a % (b in c))");
		peq(macro a * b in c, "(a * (b in c))");
		peq(macro a / b in c, "(a / (b in c))");
		peq(macro a + b in c, "(a + (b in c))");
		peq(macro a - b in c, "(a - (b in c))");
		peq(macro a << b in c, "(a << (b in c))");
		peq(macro a >> b in c, "(a >> (b in c))");
		peq(macro a >>> b in c, "(a >>> (b in c))");
		peq(macro a | b in c, "(a | (b in c))");
		peq(macro a & b in c, "(a & (b in c))");
		peq(macro a ^ b in c, "(a ^ (b in c))");
		peq(macro a == b in c, "(a == (b in c))");
		peq(macro a != b in c, "(a != (b in c))");
		peq(macro a > b in c, "(a > (b in c))");
		peq(macro a >= b in c, "(a >= (b in c))");
		peq(macro a < b in c, "(a < (b in c))");
		peq(macro a <= b in c, "(a <= (b in c))");
		peq(macro a...b in c, "(a ... (b in c))");
		peq(macro a || b in c, "(a || (b in c))");
		peq(macro a && b in c, "(a && (b in c))");
		peq(macro a => b in c, "(a => (b in c))");
		peq(macro a = b in c, "(a = (b in c))");
		peq(macro a += b in c, "(a += (b in c))");
	}

	function peq(e, s) eq(new Printer().printExpr(parentize(e)), s);

	static function parentize(e:Expr):Expr {
		return switch e.expr {
			case EConst(_): e;
			case _:
				e = ExprTools.map(e, parentize);
				{expr: EParenthesis(e), pos: e.pos};
		}
	}
}
