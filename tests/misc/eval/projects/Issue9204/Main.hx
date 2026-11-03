class C {
	@:deprecated
	static inline final SOME = 1;

	static public function f(x:Int) {
		switch (x) {
			case SOME:
				return 'hello';
			case _:
				return 'world';
		}
	}
}

class Main {
	static function main() {
		if(C.f(1) != 'hello') throw 'Test C.f(1) failed';
		if(C.f(2) != 'world') throw 'Test C.f(2) failed';
	}
}