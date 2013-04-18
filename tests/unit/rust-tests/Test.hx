import haxe.unit.*;
import rust.*;
class Test extends TestCase {
	public static function main() {
		var r = new TestRunner();
		r.add(new Test());
		r.run();
	}
	function testTuple() {
		var t = new Tuple3(371.235, "Nopenup", false);
		t.a;
		t.b;
		t.c;
	}
}