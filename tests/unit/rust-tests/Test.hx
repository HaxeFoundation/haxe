import rust.Tuple3;
import rust.Tuple2;
class Test {
	static var num:Int = 7;
	var dup:Single  = 8.990;
	public function new() {
	}
	public static function main() {
		var b = 8.9;
		var c:Single;
		c = 8;
		var d:Null<Single> = 0.8678;
		d = null;
		new Test();
		testTuple();
		for(i in 0...4)
			num += i;
	}
	static function testTuple() {
		var t = new Tuple3(371.235, 38, false);
		var p = new Tuple2("Nope", 22);
		var a = new Array();
		var s:Single = 23.663124;
		s;
		a.push(t);
		t.a;
		t.b;
		t.c;
		p.a + p.b;
	}
}