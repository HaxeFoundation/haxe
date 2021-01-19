enum E<T:String> {
	C(t:T);
}

class Main {
	static public function main() {
		E.C(12); // doesn't fail
	}
}
