package unit.issues;

#if jvm

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
	function abstractFunction():Void {}

	@:overload
	function abstractFunction(i:Int):Void {}
}

private interface InterfaceToBeImplemented {
	@:overload function toBeImplemented():Bool;
	@:overload public function toBeImplemented(i:Int):Int;
}

abstract class AbstractThatImplementsInterface implements InterfaceToBeImplemented {
	public function new() {}
}

class ConcreteChildThatImplements extends AbstractThatImplementsInterface {
	@:overload
	public function toBeImplemented() {
		return true;
	}

	@:overload
	public function toBeImplemented(i:Int) {
		return i * 2;
	}
}

#end

private interface InterfaceToBeImplementedNO {
	function toBeImplemented():Bool;
}

abstract class AbstractThatImplementsInterfaceNO implements InterfaceToBeImplementedNO {
	public function new() {}
}

class ConcreteChildThatImplementsNO extends AbstractThatImplementsInterfaceNO {
	public function toBeImplemented() {
		return true;
	}
}

abstract private class AbstractParent {
	public function new():Void {}

	abstract public function abstractFunction():Bool;
}

private class ConcreteChild extends AbstractParent {
	public function new() {
		super();
	}

	public function abstractFunction():Bool {
		return true;
	}
}

class Issue9619 extends unit.Test {
	function test() {
		#if jvm
		var cc = new ConcreteOverloadChild();
		t(HelperMacros.typeError(new AbstractOverloadParent()));

		var atii:AbstractThatImplementsInterface = new ConcreteChildThatImplements();
		t(atii.toBeImplemented());
		eq(24, atii.toBeImplemented(12));
		#end
		var cc = new ConcreteChild();

		t(HelperMacros.typeError(new AbstractParent()));
		t(cc.abstractFunction());

		var ac:AbstractParent = cc;
		t(ac.abstractFunction());

		var atiino:AbstractThatImplementsInterfaceNO = new ConcreteChildThatImplementsNO();
		t(atiino.toBeImplemented());
	}
}
