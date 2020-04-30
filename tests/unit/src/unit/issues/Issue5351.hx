package unit.issues;
import scripthost.Issue5351;

class Issue5351 extends Test {
	public function test() {
		var t3:Issue5351_2 = Type.createInstance(Type.resolveClass('unit.issues.Issue5351_3'), []);
		eq(t3.doTest1(), 'doTest1 override');
		eq(t3.doTest2(), 'doTest2 override');
		eq(t3.doTest3(), 'doTest3 override');

		eq(scripthost.Issue5351.callDoTest1(t3), 'doTest1 override');
		eq(scripthost.Issue5351.callDoTest2(t3), 'doTest2 override');
		eq(scripthost.Issue5351_2.callDoTest1(t3), 'doTest1 override');
		eq(scripthost.Issue5351_2.callDoTest2(t3), 'doTest2 override');
		eq(scripthost.Issue5351_2.callDoTest3(t3), 'doTest3 override');

    var t3 = new Issue5351_3();
		eq(t3.doTest1(), 'doTest1 override');
		eq(t3.doTest2(), 'doTest2 override');
		eq(t3.doTest3(), 'doTest3 override');

		eq(scripthost.Issue5351.callDoTest1(t3), 'doTest1 override');
		eq(scripthost.Issue5351.callDoTest2(t3), 'doTest2 override');
		eq(scripthost.Issue5351_2.callDoTest1(t3), 'doTest1 override');
		eq(scripthost.Issue5351_2.callDoTest2(t3), 'doTest2 override');
		eq(scripthost.Issue5351_2.callDoTest3(t3), 'doTest3 override');

		eq(t3.doTest4(), 'doTest4');
	}
}

@:keep class Issue5351_3 extends Issue5351_2 {
	override public function doTest1() {
		return 'doTest1 override';
	}

	override public function doTest2() {
		return 'doTest2 override';
	}

	override public function doTest3() {
		return 'doTest3 override';
	}

  public function doTest4() {
		return 'doTest4';
  }
}
