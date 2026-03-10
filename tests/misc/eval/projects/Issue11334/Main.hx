class Main {
	static function main() {
		final foo:Foo = {
			a: () -> {
				trace(123);
			},
			b: () -> {
				trace(123);
			},
			c: 123,
			d: 123,
		}
	}
}

typedef Foo = {
	a:() -> Void,
	c:Int,
}
