import haxe.unit.*;
class Test extends TestCase {
	public static function main() {
		var r = new TestRunner();
		r.add(new Test());
		r.run();
	}
}