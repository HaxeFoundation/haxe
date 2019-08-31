package unit.issues;

private enum abstract JsonTypeKind<T>(String) {
	var TMono;
}

class Issue8716 extends unit.Test {
#if !static
	function test() {
		var u:UInt = null;
		eq('null', '$u');
	}
#end
}