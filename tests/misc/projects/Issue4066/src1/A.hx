class A {

	public function new() { }

	public function doSomething() {
		return "DO SOMETHING ALREADY!";
	}

	@:astSource
	public function doSomethingElse() {
		return "Never mind, do something else.";
	}

}