// @:callSuper on abstract function is invalid — there is no implementation to call.
abstract class A {
	public function new() {}
	@:callSuper abstract public function init():Void;
}

class B extends A {
	public function init() {}
}

class Main6 {
	static function main() {}
}
