/** GrandParent class doc */
class GrandParent {
	public function new() {}

	/** GrandParent field doc **/
	public function test() {}
}

@:inheritDoc
class Parent extends GrandParent {
	@:inheritDoc override public function test() {}
}

/** Child class doc */
@:inheritDoc
class Child extends Parent {
	/** Child field doc **/
	@:inheritDoc override public function test() {}
	/** Child field 2 doc **/
	@:inheritDoc(InheritDocTypes.Unrelated.unrelated)
	static public function test2() {}
}

/** Unrelated class doc */
class Unrelated {
	/** unrelated field doc */
	static public function unrelated() {}
}