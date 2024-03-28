@:keepSub
@:keep
class BaseCase implements utest.ITest {
	var dummy:String = '';

	public function new() {}

	public function setup() {
		dummy = '';
	}

	function assert<T>(expected:Array<T>, generator:Iterator<T>, ?p:haxe.PosInfos) {
		dummy = '';
		for (it in generator) {
			Assert.equals(expected.shift(), it, p);
		}
		Assert.equals(0, expected.length, p);
	}
}