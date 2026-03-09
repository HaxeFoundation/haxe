function main() {
	var a = 1;
	inline for (x in 0...a) {}
	var a = [1, 2];
	inline for (x in a) {}
	var a:A = cast [1, 2];
	inline for (x in a) {}
	var a:A2 = cast [1, 2];
	inline for (x in a) {}
	var a = new haxe.ds.GenericStack();
	inline for(x in a) {}
	var a = [1 => 2];
	inline for(x in a) {}
	inline for (x => y in a) {}

	// working cases from here
	inline for (x in 0...3) {
		trace(x);
	}
	inline for (x in [0, 1, 2]) {
		trace(x);
	}
}

abstract A(Array<Int>) {
	public function hasNext() { return this.iterator().hasNext(); }
	public function next() { return this.iterator().next(); }
}

@:forward
abstract A2(Array<Int>) {}


