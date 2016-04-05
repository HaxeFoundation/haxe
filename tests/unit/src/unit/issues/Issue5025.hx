package unit.issues;

class Issue5025 extends Test {
	function test() {
		try {
			switch (null) {
				case Value(i):
					trace(i);
			}
		} catch (e:Dynamic) {}
	}
}

enum SomeEnum {
	Value(i:Int);
}