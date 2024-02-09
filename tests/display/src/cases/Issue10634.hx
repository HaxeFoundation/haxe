package cases;

class Issue10634 extends DisplayTestCase {
	/**
		function test(?x:String,  ?e:haxe.macro.Expr.ExprDef) {}

		function main() {
			test(EBreak);
		}
	**/
	// function test() {
	// 	arrayEq([], diagnostics());
	// }
}
