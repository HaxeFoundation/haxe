extern class Base {
	@:coroutine public function new():Void;
}

class Child extends Base {}

function main() {
	new Child();
}
