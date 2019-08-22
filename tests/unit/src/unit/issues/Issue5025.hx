package unit.issues;

class Issue5025 extends Test {
	function test() {
		noAssert();
	}

	function shouldCompile() {
		#if !(java || cs || as3 || lua)
		try {
			switch (null) {
				case Value(i):
					trace(i);
			}
		} catch (e:Dynamic) {}
		#end
	}
}

enum SomeEnum {
	Value(i:Int);
}