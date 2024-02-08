import haxe.CallStack;

class Main {
	static function main() {
		trace(new Child());
	}
}

extern class Parent {}

class Child extends Parent {
	public function new() {
		trace('hello');
	}
}