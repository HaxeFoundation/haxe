package unit.issues;

class Issue7335 extends Test {
	function test() {

		var data:Dynamic = {};
		data.foo = 1;
		var x:Float = data.foo++;

		eq(1.0, x);
		eq(2, data.foo);

		var data:Dynamic = {};
		data.foo = 1;
		var x:Float = data.foo--;
		eq(1.0, x);
		eq(0, data.foo);

		var data:Dynamic = {};
		data.foo = 1;
		var x:Float = ++data.foo;
		eq(2.0, x);
		eq(2, data.foo);

		var data:Dynamic = {};
		data.foo = 1;
		var x:Float = --data.foo;
		eq(0.0, x);
		eq(0, data.foo);

	}
}
