package unit.issues;

class Issue4343 extends Test {
	function test() {
		(new SomeObject() : ISomeObject).call();
	}
}

class SomeObject implements ISomeObject {
	public function new() {}
	public function call():Void {}
}

interface ISomeObject extends IEmtpyExtendsCallable extends IEmpty {}

interface IEmpty {}

interface IEmtpyExtendsCallable extends ICallable {}

interface ICallable {
	public function call():Void;
}