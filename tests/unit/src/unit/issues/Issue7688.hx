package unit.issues;

class Issue7688 extends unit.Test {
	#if !macro
	function test() {
		var bar = 'bar';
		nullify(@blargh { foo: 'bar$bar' });//compiles just fine
		nullify(@blargh { foo: 'bar$whatever' });//also compiles just fine, because the whole expression is actually discarded
		nullify({ foo: 'bar$bar' });//unknown identifier bar
		utest.Assert.pass();
	}
	#end
	static macro function nullify(e) {
		return macro null;
	}
}