interface A {
	public function a():String;
}

interface B extends A {

}

interface C extends B {
	public function a():Int;
}

class Main {
	static function main() {

	}
}