package scripthost;

@:keep class Issue5351 {
	public function new() {
		//initialize variables
	}

	public function doTest1() {
		return 'doTest1';
	}

	public function doTest2() {
		return 'doTest2';
	}

	public static function callDoTest1(i:Issue5351) {
		return i.doTest1();
	}

	public static function callDoTest2(i:Issue5351) {
		return i.doTest2();
	}
}

@:keep class Issue5351_2 extends Issue5351 {
	public function doTest3() {
		return 'doTest3';
	}

	public static function callDoTest1(i:Issue5351_2) {
		return i.doTest1();
	}

	public static function callDoTest2(i:Issue5351_2) {
		return i.doTest2();
	}

	public static function callDoTest3(i:Issue5351_2) {
		return i.doTest3();
	}
}
