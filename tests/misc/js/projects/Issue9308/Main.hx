enum E {
	A;
	B;
}

class Main {
	static var n:Int = 3;
	static var e:Null<E> = A;

	static function main() {
		switch (e) {
			case A:
				// commenting this trace makes it work...
				trace("hi");

				for (i in 0...n) {
					trace(i);
				}

			case B:
				for (i in 0...n) {
					trace(i);
				}

			// commenting this default removes the nullcheck for `e`
			// and makes it work...
			default:
		}
	}
}