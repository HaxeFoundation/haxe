class Main extends Parent {
	static function main() {}

	@:native('childNative')
	override function some() {
		super.some();
	}

	@:native('childNative2')
	override function noNative() {
		super.noNative();
	}
}

class Parent {
	@:native('parentNative')
	public function some() {
	}

	public function noNative() {
	}
}