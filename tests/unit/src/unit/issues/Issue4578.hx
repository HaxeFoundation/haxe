package unit.issues;

private class TList {}
private class TCons<U, V:TList> extends TList {}
private class TNil extends TList {}

private enum Stack<L:TList> {
	Nil:Stack<TNil>;
	Cons<X, L:TList>
	(x : X, xs : Stack<L>) : Stack<TCons<X, L>>;
}

private interface Instr<L1:TList, L2:TList> {
	function denote(s:Stack<L1>):Stack<L2>;
}

private class IUnOp<X, Y, S:TList> implements Instr<TCons<X, S>, TCons<Y, S>> {
	var f:X->Y;

	public function new(f) {
		this.f = f;
	}

	public function denote(s:Stack<TCons<X, S>>):Stack<TCons<Y, S>> {
		return switch (s) {
			case Cons(x, s):
				Cons(f(x), s);
		}
	}
}

@:haxe.warning("-WGenerator")
class Issue4578 extends Test {
	function test() {
		var i = new IUnOp(function(x) return x * 2);
		var v = i.denote(Cons(10, Nil));
		eq(20, getHead(v));
	}

	static function getHead<S, T:TList>(s:Stack<TCons<S, T>>):S {
		return switch (s) {
			case Cons(x, _): x;
		}
	}
}
