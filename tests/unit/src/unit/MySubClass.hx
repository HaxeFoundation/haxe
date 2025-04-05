package unit;

class MySubClass extends MyClass {

	public override function get() {
		return val * 2;
	}

	@:keep static var XXX = 3;

}