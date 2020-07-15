package unit.issues;

#if (java || cs)

private abstract class AbstractOverloadParent {
	public function new():Void {}

	@:overload
	abstract function abstractFunction():Void;

	@:overload
	abstract function abstractFunction(i:Int):Void;
}

private class ConcreteOverloadChild extends AbstractOverloadParent {
	public function new() {
		super();
	}

	@:overload
	override function abstractFunction():Void {}

	@:overload
	override function abstractFunction(i:Int):Void {}
}

#end

abstract private class AbstractParent {
	public function new():Void {}

	abstract public function abstractFunction():Bool;
}

private class ConcreteChild extends AbstractParent {
	public function new() {
		super();
	}

	override public function abstractFunction():Bool {
		return true;
	}
}

class Issue9619 extends unit.Test {
	function test() {
		#if (java || cs)
		var cc = new ConcreteOverloadChild();
		t(HelperMacros.typeError(new AbstractOverloadParent()));
		#end
		var cc = new ConcreteChild();

		t(HelperMacros.typeError(new AbstractParent()));
		t(cc.abstractFunction());

		var ac:AbstractParent = cc;
		t(ac.abstractFunction());
	}
}