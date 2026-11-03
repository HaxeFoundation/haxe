enum E<T> {
	None;
	Some(v:T);
}

class Main {
	static function foo(v:E<Int>) {
		var a = None;
		return v.match(a);
	}

	static function main() {
		trace(foo(None));
		trace(foo(Some(55)));
	}
}
