package unit.issues;
import unit.Test;

class Issue9593 extends Test {
	function test() {
		foo(function(res) res.error);
		utest.Assert.pass();
	}

	static function foo<T:{}>(cb:T->Void):Void {}
}
