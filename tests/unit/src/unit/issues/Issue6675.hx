package unit.issues;

private enum E {
	Int( v : Int );
	Add( a : E, b : E );
}

class Issue6675 extends unit.Test {

	static function op( op : E ) { }

	function test() {
		inline function foo(bop,a,b) {
			return op(bop(a,b));
		}
		foo(Add,Int(0),Int(1));
		noAssert();
	}
}