package unit.issues;

private enum abstract JsonTypeKind<T>(String) {
	var TMono;
}

class Issue6880 extends unit.Test {
	function test() {
		var u:Null<AInt> = null;
		eq('null', '$u');
	}
}

private abstract AInt(Int) from Int {
	public inline function toString() {
		var result = this + 100;
		return '$result';
	}
}