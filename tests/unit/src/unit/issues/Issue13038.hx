package unit.issues;

@:keep private abstract class Base {
	public function new() {}
	public abstract function f( x : Int, flag : Bool = false ) : Int;
	public abstract function g( x : Int, ?name : String ) : Int;
}

private abstract class BaseFoo {
	public function new() {}
	public abstract function fbool1( x : Int, flag : Bool = false ) : Int;
	public abstract function fbool2( x : Int, flag : Bool = false ) : Int;
	public abstract function fbool3( x : Int, flag : Bool = true ) : Int;
	public abstract function fint1( x : Int, i : Int = 0 ) : Int;
	public abstract function fint2( x : Int, i : Int = 0 ) : Int;
	public abstract function fint3( x : Int, i : Int = 10 ) : Int;
}

private class Foo extends BaseFoo {
	public function fbool1( x : Int, flag : Bool = false ) : Int {
		return x + (flag ? 10 : 1);
	}
	public function fbool2( x : Int, flag : Bool = true ) : Int {
		return x + (flag ? 10 : 1);
	}
	public function fbool3( x : Int, flag : Bool = false ) : Int {
		return x + (flag ? 10 : 1);
	}
	public function fint1( x : Int, i : Int = 0 ) : Int {
		return x + i;
	}
	public function fint2( x : Int, i : Int = 10 ) : Int {
		return x + i;
	}
	public function fint3( x : Int, i : Int = 0 ) : Int {
		return x + i;
	}
}

class Issue13038 extends Test {
	@:keep static function callAbstract( b : Base ) {
		return b.f(1) + b.f(1, true) + b.g(1) + b.g(1, "ab");
	}
	static function callAbstract1( b : BaseFoo ) {
		return b.fbool1(100) + b.fbool1(100, true) + b.fint1(100) + b.fint1(100, 15);
	}
	static function callAbstract2( b : BaseFoo ) {
		return b.fbool2(100) + b.fbool2(100, true) + b.fint2(100) + b.fint2(100, 15);
	}
	static function callAbstract3( b : BaseFoo ) {
		return b.fbool3(100) + b.fbool3(100, true) + b.fint3(100) + b.fint3(100, 15);
	}
	function test() {
		eq(true, callAbstract != null);
		var foo = new Foo();
		eq(426, callAbstract1(foo));
		eq(445, callAbstract2(foo));
		eq(426, callAbstract3(foo));
	}
}
