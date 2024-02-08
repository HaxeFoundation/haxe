package unit.issues;
import unit.Test;

class Issue9593 extends Test {
	function test() {
		foo(function(res) res.error);
		utest.Assert.pass();
	}

	function test2() {
		foo(function(res) {
			res.error;
			res.file;
		});
		utest.Assert.pass();
	}

	static function foo<T:{}>(cb:T->Void):Void {}
}
