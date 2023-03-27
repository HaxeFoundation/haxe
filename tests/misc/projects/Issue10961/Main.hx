class Main {
	static function main() {
		final foo:Foo = {
			a: 0,
			b: 0,
			c: 0,
			a: 0,
		}
		final foo = {
			a: 0,
			b: 0,
			c: 0,
			a: 0,
		}
		final foo:CFoo = {
			a: 0,
			b: 0,
			a: 0,
		}
	}
}

typedef Foo = {
	a:Int,
	b:Int,
	c:Int,
}

typedef FooAdd = {
	d:Int,
}

@:structInit
class CFoo {
	var a:Int;
	var b:Int;
}
