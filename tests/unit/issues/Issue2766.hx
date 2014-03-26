package unit.issues;
import unit.Test;

private class Foo {
    public var foo = "foo";
    public function new () {}
}

class Issue2766 extends Test {

	function test() {
		eq("foo", getValue());
	}
	
    macro static function getValue() {
        return macro $v{new Foo().foo};
    }
}