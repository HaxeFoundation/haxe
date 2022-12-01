class Main {
	static final numbers = [];
	static function addNum(n:Int):Int {
		numbers.push(n);
		return n;
	}
	// unreferenced, but should be preserved from DCE due to the side effect
	static final node = addNum(1);

	static function main() {
		trace(numbers.length);
	}
}
