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
	/** Child doc **/
	@:inheritDoc(misc.InheritDocTypes.Unrelated.unrelated)
	override public function test2() {}
}

class Unrelated {
	/** unrelated doc */
	static public function unrelated() {}
}