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

class Foo implements IFoo implements IFoo2 extends Parent {
	/** Foo doc **/
	@:inheritDoc override public function test():Void {}
}

class Foo2 implements IFoo implements IFoo2 {
	public function new() {}

	/** Foo doc **/
	@:inheritDoc public function test():Void {}
}

class Foo3 implements IFoo implements IEmptyFoo {
	public function new() {}

	@:inheritDoc public function test():Void {}
}

class Foo3Inv implements IEmptyFoo implements IFoo {
	public function new() {}

	@:inheritDoc public function test():Void {}
}

interface IEmptyFoo {
	function test():Void;
}

interface IFoo {
	/** IFoo doc **/
	function test():Void;
}

interface IFoo2 {
	/** IFoo2 doc **/
	function test():Void;
}
