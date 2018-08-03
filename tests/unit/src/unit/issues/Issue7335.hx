package unit.issues;

class Issue7335 extends Test {
	function test() {
		#if !python
		var data:Dynamic = {};
		data.foo = 1;
		data.foo++;
		eq(2, data.foo);
		#end
	}
}
