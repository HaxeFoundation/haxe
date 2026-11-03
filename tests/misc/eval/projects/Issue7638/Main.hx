class Main {
	static function main() {}
}

class Parent {
	dynamic function dynMethod() {}
}

class Child extends Parent {}

class GrandChild extends Child {
	override function dynMethod() {
		super.dynMethod();
	}
}