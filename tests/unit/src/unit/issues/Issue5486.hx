package unit.issues;

import haxe.ds.Option;

class Issue5486 extends unit.Test {
	function test() {
		function check(v) {
			switch v {
				case Some(10): noAssert();
				case _: assert();
			}
		}

		var inputDynamic:Dynamic = 10;
		var inputNullDynamic:Null<Dynamic> = 10;
		var x:Option<Int>;
		x = Some(inputDynamic);
		check(x);
		x = Some(inputNullDynamic);
		check(x);

		check(broken(10));
	}

	static function broken(?input:Dynamic):Option<Int>{
		if(Std.isOfType(input, Int)){
			return Some(input);
		} else {
			return None;
		}
	}
}