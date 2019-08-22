class Closures1 {
	static var tmp:Any;

	static public function main() {
		var inst = new Test();
		js.Syntax.code('global.inst = {0}', inst);
		tmp = inst.test1;
	}
}

@:keep
class Test {
	public function new() {}

	public function test1() {
		return 1;
	}
	public function test2() {
		return 2;
	}
}