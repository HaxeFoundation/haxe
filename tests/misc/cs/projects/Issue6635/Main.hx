abstract A<V>(String) {}

typedef S = {a:A<Int>};

class Main {
	function new(s:S) {}

	static function getS():S return null;

	static function main() {
		new Main(getS());
	}
}