class A {
	public function f():Array<A> {
		return [];
	}

	public function f2():A {
		return this;
	}
}

@:generic
class B<T:haxe.Constraints.Constructible<()->Void>> {
	var items:Map<Int,A>;

	public function new() {
		items = new Map();
	}

	public function f():Array<A> {
		var a = [];
		for (node in items)
			a = a.concat(node.f());
		return a;
	}
}

class Main {
	static function main() {
		var b = new B<Main>();
		Sys.stderr().writeString("" + b.f());
	}

	public function new() { }
}