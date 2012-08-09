package unit;

class MySubClass extends MyClass {

	public override function get() {
		return val * 2;
	}

	@:keep #if as3 public #end static var XXX = 3;

}