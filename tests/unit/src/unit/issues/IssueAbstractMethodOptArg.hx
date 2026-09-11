package unit.issues;

@:keep private abstract class Base {
	public function new() {}
	public abstract function f( x : Int, flag : Bool = false ) : Int;
	public abstract function g( x : Int, ?name : String ) : Int;
}

class IssueAbstractMethodOptArg extends Test {
	@:keep static function callAbstract( b : Base ) {
		return b.f(1) + b.f(1, true) + b.g(1) + b.g(1, "ab");
	}

	function test() {
		eq(true, callAbstract != null);
	}
}
