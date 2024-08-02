class Main {
	static function main() {
		var future = new Future();
		future.eager();
	}
}

abstract Future({}) from {} {
	public function new()
		this = {};

	public inline function eager():Future {
		trace("much side effect!");
		return this;
	}
}
