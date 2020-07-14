abstract
class Abstract {
	public function new() {}
	@:abstract
	function abstractFunction():Void;
}

class Main {
	static function main() {
		new Abstract();
	}
}
