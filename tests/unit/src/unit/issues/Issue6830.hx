package unit.issues;

private typedef VarChar<@:const L> = String;

class Issue6830 extends unit.Test {
	function test() {
		testMacro();
	}

	static macro function testMacro() {
		var ct = haxe.macro.TypeTools.toComplexType(haxe.macro.Context.typeof(macro (null: VarChar<123>)));
		return macro t($v{ct.match(TPath({params: [TPExpr({expr: EConst(CInt("123"))})]}))});
	}
}
