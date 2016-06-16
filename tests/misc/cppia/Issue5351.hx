class Issue5351 extends haxe.unit.TestCase {
	public function test() {
		var t3:I5351Test2 = Type.createInstance(Type.resolveClass('I5351Test3'), []);
		assertEquals(t3.doTest1(), 'doTest1 override');
		assertEquals(t3.doTest2(), 'doTest2');
		assertEquals(t3.doTest3(), 'doTest3');
	}
}

class I5351Test {

	public function new() {
		//initialize variables
	}

	public function doTest1() {
		return 'doTest1';
	}

	public function doTest2() {
		return 'doTest2';
	}
}

class I5351Test2 extends I5351Test {
	public function doTest3() {
		return 'doTest3';
	}
}

#if cppia
class I5351Test3 extends I5351Test2 {
	override public function doTest1() {
		return 'doTest1 override';
	}
}
#end
