package unit.issues.misc.issue8543.hx;

class Sample {
	@:pure(false)
	static public function test() {
		return 'hello';
	}
}