package unit.issues;

import utest.Assert;

@:generic
private class Temp<T> {
	var t:T;

	public function new(t:T) {
		this.t = t;
	}
}

class Issue5482 extends Test {
	@:generic
	static function makeTemp<T>(c:T):Temp<T>
		return new Temp<T>(c);

	@:generic
	static function makeTempArray<T>(c:T):Array<Temp<T>>
		return [new Temp<T>(c)];

	function test() {
		var tt:Temp<Int> = makeTemp(10);
		var tt1 = makeTemp(10);

		var tt:Array<Temp<Int>> = makeTempArray(10);
		var tt1 = makeTempArray(10);

		Assert.pass();
	}
}
