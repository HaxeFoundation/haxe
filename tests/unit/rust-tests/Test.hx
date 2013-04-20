import rust.Tuple3;
import rust.Tuple2;
class Test {
	static var num:Int = 7;
	public static function main() {
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