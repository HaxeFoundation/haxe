interface I {
	@:flash.property var x(get,never):Int;
}

class Main implements I {
	public var x(get,never):Int;
	function get_x():Int return 15;
}