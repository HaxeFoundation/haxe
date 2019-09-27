package misc;

class GrandParent {
	public function new() {}

	/** GrandParent doc **/
	public function test() {}
}

class Parent extends GrandParent {
	@:inheritDoc override public function test() {}
}

class Child extends Parent {
	/** Child doc **/
	@:inheritDoc override public function test() {}
}