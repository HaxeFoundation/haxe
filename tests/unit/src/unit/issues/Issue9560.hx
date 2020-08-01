package unit.issues;

import haxe.Constraints.Function;

class Issue9560 extends unit.Test {
	public function test() {
		var t:ERegAbs<Int->Int, ~/foo/i> = a -> a + a;
		utest.Assert.pass();
	}
}

private abstract ERegAbs<T:Function, @:const R:EReg>(T) {
    @:from private static function fromFunction<T:Function>(v:T):ERegAbs<T, ~//> return cast v;
}