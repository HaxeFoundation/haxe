import rust.StdTypes;
class Test {
	public static function main() {
		var a:Null<Int> = 46;
		var func:Int -> Null<Int>;
		func = function(n):Null<Int> {
			return n;
		};
		func(24);
	}
}