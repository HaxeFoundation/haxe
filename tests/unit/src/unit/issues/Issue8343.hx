package unit.issues;

using unit.issues.Issue8343.DummyClassTools;
using unit.issues.Issue8343.DummyAbstractTools;

class Issue8343 extends unit.Test {
	function test() {
		for(i in (null:DummyClass)) {
			eq(i, 1);
		}

		for(i in ("hello":DummyAbstract)) {
			eq(i, 1);
		}
	}
}

class DummyClassTools {
	static public function iterator(m:DummyClass):Iterator<Int> {
		return [1].iterator();
	}
}

class DummyAbstractTools {
	static public function iterator(m:DummyAbstract):Iterator<Int> {
		return [1].iterator();
	}
}

private class DummyClass implements ArrayAccess<String> {
	public var length:Int;
}

private abstract DummyAbstract(String) from String {
	public var length(get,never):Int;
	function get_length() return this.length;

	@:arrayAccess function get(i:Int) return this.charAt(i);
}

