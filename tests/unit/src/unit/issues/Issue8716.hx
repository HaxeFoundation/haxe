package unit.issues;

private enum abstract JsonTypeKind<T>(String) {
	var TMono;
}

class Issue8716 extends unit.Test {
#if !static
	function test() {
		var u:AInt = null;
		eq('null', '$u');
	}
#end
}

private abstract AInt(Int) from Int {
	public inline function toString() {
		var result = this + 100;
		return '$result';
	}
}