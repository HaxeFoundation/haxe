package misc;

/** GrandParent class doc */
class GrandParent {
	public function new() {}

	/** GrandParent doc **/
	public function test() {}
}

@:inheritDoc
class Parent extends GrandParent {
	@:inheritDoc override public function test() {}
}

/** Child class doc */
@:inheritDoc
class Child extends Parent {
	/** Child doc **/
	@:inheritDoc override public function test() {}
}