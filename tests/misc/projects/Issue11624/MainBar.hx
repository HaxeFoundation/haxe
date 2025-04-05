interface Foo {
	function bar<T>():T;
}

class Bar implements Foo {
	public function bar<T>() return null; // Error: write_full_path hxb writer failure
}

function main() {}