package unit.issues;

class Issue7659 extends unit.Test {
	function test() {
		var a:DummyAbstract = ['hello'];
		var actual = [for(i in a) i];
		aeq([1, 2, 3], actual);
	}
}

private abstract DummyAbstract(Array<String>) from Array<String> {
	public var length(get,never):Int;
	function get_length() return this.length;
	@:arrayAccess function get(i:Int) return this[i];

	public function iterator() return [1, 2, 3].iterator();
}

