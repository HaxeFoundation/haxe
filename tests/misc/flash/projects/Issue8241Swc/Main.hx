private class PropClassChild extends PropClass {
	override function get_x():Int {
		return super.get_x() + 1;
	}

	override function set_x(value:Int) {
		return super.set_x(value + 1);
	}
}

private class PropIfaceImpl implements PropIface {
	var _x = 42;
	@:flash.property
	public var x(get,set):Int;
	function get_x():Int return _x;
	function set_x(v:Int):Int return _x = v;
	public function new() {}
}

private interface HaxePropIface {
	var x(get,set):Int;
}

private class HaxePropIfaceImpl extends PropClass implements HaxePropIface {}

class Main {
	static function main() {
		var p = new PropClass();
		var pc = new PropClassChild();
		var pi = new PropIfaceImpl();
		var pi2:PropIface = new PropIfaceImpl();
		var hp = new HaxePropIfaceImpl();
		var hp2:HaxePropIface = new HaxePropIfaceImpl();
	}
}
