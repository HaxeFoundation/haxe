package;

@:coreApi
class A {

	public function new() { }

	public function doSomething():String {
		return "ok";
	}

	public function doSomethingElse():String @:useAstSource @:coreApi A.doSomethingElse;
}