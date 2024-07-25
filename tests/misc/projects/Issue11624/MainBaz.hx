interface Foo {
	function baz<T>():T;
}

class Bar implements Foo {
	public function baz<T>():T return null; // this is fine
}

function main() {}