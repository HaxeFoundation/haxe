package unit.issues;
import unit.Test;

class Issue2755Class {
	public var initialized:Int = 666;

	public function new () {

	}
}

class Issue2755 extends Test {
	function test() {
		var f:Issue2755Class = Type.createInstance(Type.resolveClass("unit.issues.Issue2755Class"), []);
		eq(666, f.initialized);
	}
}