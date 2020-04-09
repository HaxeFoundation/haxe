class C {
	@:deprecated
	static inline final SOME = 1;

	static public function f(x:Int) {
		switch (x) {
			case SOME:
				trace(1);
			case _:
				trace(2);
		}
	}
}

class Main {
	static function main() {
		C.f(1);
		C.f(2);
	}
}