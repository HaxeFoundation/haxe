import rust.StdTypes;
using StringTools;
interface STest {

}
class Test<V:STest> implements STest {
	var value:V;
	public function new(v:V) {
		value = v;
	}
	public static function main() {
		var ot = new Test(null);
		var t:Relation = {id: "Goodbye, world!", value: 23};
		var oi:Int8 = 24;
		oi += cast(5, Int8);

		Sys.println("hello, world!"+t.value);
	}
}
typedef Relation = {
	id:String,
	value:Int
}