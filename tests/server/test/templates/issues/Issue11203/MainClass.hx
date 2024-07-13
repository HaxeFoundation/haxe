class Main {
	static function main() {
		var future = new Future();
		future.eager();
	}
}

class Future {
	public function new() {}

	public inline function eager():Future {
		trace("much side effect!");
		return this;
	}
}
