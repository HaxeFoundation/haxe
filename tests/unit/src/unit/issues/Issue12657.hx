package unit.issues;

class Issue12657 extends Test {
	function test() {
		var d : Dynamic = 1;
		var ni : Null<Int> = 1;
		var nf : Null<Float> = 1.;
		t( d == ni );
		f( d > ni );
		f( d < ni );
		t( d == nf );
		f( d > nf );
		f( d < nf );
	}
}
