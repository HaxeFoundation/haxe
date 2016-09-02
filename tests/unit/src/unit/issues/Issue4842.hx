package unit.issues;

class Issue4842 extends Test {
	function test() {
		var i:Int = 0;
		f(isNull(i));
		var u:UInt = 0;
		f(isNull(u));
		var i64:haxe.Int64 = haxe.Int64.make(0, 0);
		f(isNull(i64));
	}

	static inline function isNull<T>(maybeNull:Null<T>):Bool return {
		maybeNull == null;
	}
}