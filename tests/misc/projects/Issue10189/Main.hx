enum E {
	A;
	B;
	C(v:Int);
}

class Main {
	static function main() {
		var e = A;
		switch (e) {
			case A:
			case B:
			case C(0):
			case C(x) if (x > 0):
		}
	}
}
