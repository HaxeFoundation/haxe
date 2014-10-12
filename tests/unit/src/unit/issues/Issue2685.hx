package unit.issues;
import unit.Test;

private abstract A({a:Int}) from {a:Int} {
    @:to function toInt():Int return this.a * 4;
    @:to function toDynamic():Dynamic return this.a * 2;
}

class Issue2685 extends Test {
	function test() {
		var a:A = {a: 1};
		eq(2, func1(a));
		eq(4, func2(a));
	}

	static function func1(a:Dynamic) {
		return a;
	}

	static function func2(a:Int) {
		return a;
	}
}