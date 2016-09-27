package unit.issues;

private abstract Maybe<T>(Null<T>) from Null<T> {
	public inline function or(def:T):T {
		return if (this != null) this else def;
	}
}

class Issue4740 extends Test {
	function test() {
		var session:Int = (null : Maybe<Null<Int>>).or(0);
		eq(0, session);
	}
}