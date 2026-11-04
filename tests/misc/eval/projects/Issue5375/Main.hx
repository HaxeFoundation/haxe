enum E {
	A;
	B( x : Int );
	C;
}

class Main {
	static function main() {
		var e : E = null;
		switch( e ) {
			case A:
			case B:
			case C:
		}
	}
}