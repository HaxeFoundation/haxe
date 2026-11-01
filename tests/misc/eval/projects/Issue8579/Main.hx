abstract A(Int) {
	public static final instance:A = cast 42;
}

class Main {
	static function main() {
		switch 42 {
			case A.instance:
				trace("YES");
			case _:
				trace("NO");
		}
	}
}