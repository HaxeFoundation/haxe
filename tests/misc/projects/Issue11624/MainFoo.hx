interface Foo {
	function foo<T>():T;
}

class Bar implements Foo {
	public function foo() return null; // Warning: Unbound type parameter foo.T
}

function main() {}