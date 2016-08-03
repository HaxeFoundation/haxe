package unit.issues;
import scripthost.Issue5351;

class Issue5351 extends Test {
	public function test() {
		var t3:Issue5351_2 = Type.createInstance(Type.resolveClass('unit.issues.Issue5351_3'), []);
		eq(t3.doTest1(), 'doTest1 override');
		eq(t3.doTest2(), 'doTest2');
		eq(t3.doTest3(), 'doTest3');
	}
}

@:keep class Issue5351_3 extends Issue5351_2 {
	override public function doTest1() {
		return 'doTest1 override';
	}
}
