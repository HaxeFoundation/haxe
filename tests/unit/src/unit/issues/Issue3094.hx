package unit.issues;

class Issue3094 extends Test {
	static inline function isNull<T>(maybeNull:Null<T>):Bool return {
		maybeNull == null;
	}

	function test() {
		var i:Int = 0;
		f(isNull(i));
		var u:UInt = 0;
		f(isNull(u));
	}
}