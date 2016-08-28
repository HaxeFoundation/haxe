package unit.issues;

class Issue4988 extends Test {
	function test() {
		#if !(php || lua)
		try {
			var d:{i:Null<Int>} = null;
			foo(d.i > 0);
		} catch (e:Dynamic) {}
		#end
	}

	function foo(_) {}
}