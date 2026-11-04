class Closures2 {
	static var test2:()->Int;

	static public function main() {
		var inst:Test2 = js.Syntax.code('global.inst');
		test2 = inst.test2;
		if(test2() != 2) {
			throw 'Cross-module closure binding failed.';
		}
	}
}

extern class Test2 {
	public function test1():Int;
	public function test2():Int;
}