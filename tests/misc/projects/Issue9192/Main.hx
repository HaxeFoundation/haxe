class Main {
	static function main() {
		A.field;
	}
}

@:deprecated
class A {
	public static final field = 3;
}
