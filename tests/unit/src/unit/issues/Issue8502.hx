package unit.issues;

class Issue8502 extends Test {
#if cpp
  public function test() {
		var t:scripthost.Issue8502 = Type.createInstance(Type.resolveClass('unit.issues.Issue8502_2'), []);
    eq(t.doTest1(25), 'cppia 25');
    eq(t.doTest2(25), 'cppia 25');
    eq(t.doTest3(25), 'cppia 25');
    eq(t.doTest4(25), 'cppia 25');
    eq(t.doTest5(25), 'cppia 25');
    eq(t.doTest3u(25), 'cppia 25');
    eq(t.doTest4u(25), 'cppia 25');
    eq(t.doTest5u(25), 'cppia 25');
  }
#end
}

#if cpp
class Issue8502_2 extends scripthost.Issue8502 {
	override public function doTest1(f:cpp.Float32):String {
		return 'cppia ' + super.doTest1(f);
	}

	override public function doTest2(f:cpp.Float64):String {
		return 'cppia ' + super.doTest2(f);
	}

	override public function doTest3(f:cpp.Int8):String {
		return 'cppia ' + super.doTest3(f);
	}

	override public function doTest4(f:cpp.Int16):String {
		return 'cppia ' + super.doTest4(f);
	}

	override public function doTest5(f:cpp.Int32):String {
		return 'cppia ' + super.doTest5(f);
	}

	override public function doTest3u(f:cpp.UInt8):String {
		return 'cppia ' + super.doTest3u(f);
	}

	override public function doTest4u(f:cpp.UInt16):String {
		return 'cppia ' + super.doTest4u(f);
	}

	override public function doTest5u(f:cpp.UInt32):String {
		return 'cppia ' + super.doTest5u(f);
	}
}

#end