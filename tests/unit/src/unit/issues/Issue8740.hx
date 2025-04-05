package unit.issues;

class Issue8740 extends unit.Test {
	function test() {
		eq(42.0, ("":DateTime));
	}
}

private abstract DateTime(Float) from Float to Float {
	@:from
	static public inline function fromString(str:String):DateTime {
		var day:Null<Int> = 22;
		return make(day);
	}

	static public inline function make(day:Int = 1):DateTime {
		return (day - 1) * 2;
	}
}