package unit.issues;

private typedef Recursive<F> = Recursive<F>->F;
private typedef YDef<P, R> = Recursive<P->R>;

@:callable private abstract Y<P, R>(YDef<P, R>) from YDef<P, R> to YDef<P, R> {
	@:noUsing static public inline function lift<P, R>(self:YDef<P, R>):Y<P, R> {
		return new Y(self);
	}

	static public function unit<P, R>():Y<P, R> {
		return function(fn:Recursive<P->R>):P->R return fn(fn);
	}

	@:noUsing static public function pure<P, R>(f:P->R):Y<P, R> {
		return function(fn:Recursive<P->R>) return f;
	}

	public function new(self:YDef<P, R>) {
		this = self;
	}

	public function reply():P->R {
		return this(this);
	}

	public function reverse(that:YDef<P, R>) {
		return that(this);
	}
}

private typedef Recursive2<F> = Recursive2<F>->F;

function unit() {
	return function(fn:Recursive2<Any>) return fn(null);
}

class Issue12539 extends Test {
	function testFull() {
		final a = Y.unit();
		utest.Assert.pass();
	}

	function testSimple() {
		unit();
		utest.Assert.pass();
	}
}
