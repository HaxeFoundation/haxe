class Test {
	public static function main() {
		Sys.println("Hello, world!");
		var a = Std.parseInt("8");
		var b = Std.int(2.28);
		var c = Std.int(a / b);
		useless(78.9);
		Sys.println(Std.string(TestEnum.c));
	}
	static function useless<T>(v:T):T {
		return v;
	}
}
enum TestEnum {
	a;
	b;
	c;
}