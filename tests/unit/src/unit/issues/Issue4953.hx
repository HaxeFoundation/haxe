package unit.issues;

private enum E {
	Foo( v : Int );
	Other;
}

class Issue4953 extends Test {

	function foo() {
		return Foo;
	}

    function test() {
        switch( foo()(5) ) {
		case Foo(v): eq(v,5);
		default: t(false);
		}
    }

}
