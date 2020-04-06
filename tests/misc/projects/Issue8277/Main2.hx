typedef C = { foo: Int }

enum E {
	A(v:C);
}

class Main {
	static final ident:C = null;

	static function f(e:E):Int {
		return switch e {
			case A(ident): 0;
		}
	}

	static function main() {}
}