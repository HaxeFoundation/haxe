class Main2 {
	function new() {}

	function inst<T>(x:T):T return x;

	static function main() {
		new Main2().inst<Int>(1);
	}
}
