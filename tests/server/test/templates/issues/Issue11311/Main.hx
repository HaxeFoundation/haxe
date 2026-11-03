using Lambda;

abstract KeyMap(Map<Int, Bool>) {}

class Main {
	static function main() {
		log("Hello, world!");
		// log([0 => 0].empty());
	}
}

@:pure(false)
function log(v:Any):Void {}
