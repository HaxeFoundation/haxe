package unit.issues;

class Issue3742 extends Test {
	function test() {
		eq( Std.parseFloat(".05"), .05 );
		eq( Std.parseFloat(".005"), .005 );
		eq( Std.parseFloat(".5"), .5 );
	}
}

