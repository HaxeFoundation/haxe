package unit.issues;

import haxe.macro.Context.*;
import haxe.ds.Option;

#if macro
class Issue9828 {
	static macro function foo() {
		var t = typeExpr(macro {
			var y:Option<String> = x;
			switch y {
				case Some(v): v;
				case None: '';
			}
		});
		return storeTypedExpr(t);
	}
}
#else
class Issue9828 extends unit.Test {
	var x = Some('hello');

	function test() {
		eq('hello', foo());
	}

	static macro function foo(e:haxe.macro.Expr):haxe.macro.Expr;
}
#end