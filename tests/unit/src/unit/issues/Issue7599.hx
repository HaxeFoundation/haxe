package unit.issues;

#if (java || cs)

@:keep
private class Overloader {
	public function new() {

	}

	public function toString() {
		return "Overloader!";
	}

	@:generic static public function genericMe<T>(t:T) {
		return test(t);
	}

	@:generic static public function genericMeMember<T>(m:Overloader, t:T) {
		return m.testMember(t);
	}

	overload static public function test(d:Dynamic) {
		return "Dynamic: " + d;
	}

	overload static public function test(i:Int) {
		return "Int: " + i;
	}

	overload static public function test(s:String) {
		return "String: " + s;
	}

	overload public function testMember(d:Dynamic) {
		return "Dynamic: " + d;
	}

	overload public function testMember(i:Int) {
		return "Int: " + i;
	}

	overload public function testMember(s:String) {
		return "String: " + s;
	}
}

#end

class Issue7599 extends unit.Test {
	#if (java || cs)

	function testGeneric() {
		var overloader = new Overloader();
		eq("String: foo", Overloader.genericMe("foo"));
		eq("Int: 12", Overloader.genericMe(12));
		eq("Dynamic: Overloader!", Overloader.genericMe(overloader));
		eq("String: foo", Overloader.genericMeMember(overloader, "foo"));
		eq("Int: 12", Overloader.genericMeMember(overloader, 12));
		eq("Dynamic: Overloader!", Overloader.genericMeMember(overloader, overloader));
	}

	#end
}