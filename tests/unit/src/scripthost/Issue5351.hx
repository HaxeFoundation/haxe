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
}

@:keep class Issue5351_2 extends Issue5351 {
	public function doTest3() {
		return 'doTest3';
	}
}
