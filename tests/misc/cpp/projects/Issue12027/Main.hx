function main() {
	foo(new Foo());
}

function foo(foo:Null<Foo>) {
	(foo ?? throw "hello")[0]++;
}

abstract Foo(Int) {
	public function new() {
		this = 0;
	}

	@:op([]) function get(i:Int) {
		return this;
	}

	@:op([]) function set(i:Int, val:Int) {
		return this;
	}
}
