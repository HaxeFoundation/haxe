function main() {
	var foo:Foo = 42;

	switch (foo) {
		case LCTRL:
		case _:
	}
}

enum abstract Foo(Int) from Int to Int {
	var LCTRL:Foo = 224 | (1<<30);
}
