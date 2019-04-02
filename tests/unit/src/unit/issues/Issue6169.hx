package unit.issues;

class Rec {
	var x : Int = 0;
	public inline function new(rec:Bool) {
		if ( rec ) {
			var r = new Rec(false);
			x = r.x;
		}
	}
}

class Issue6169 extends unit.Test {
	function test() {
		var r = new Rec(false);
		noAssert();
	}
}