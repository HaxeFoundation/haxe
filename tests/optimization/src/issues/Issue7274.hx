package issues;

private interface Show<T> {
	function show(fa:T):String;
}

private class ShowApi {
	public static inline function show<A>(a:A, ?S:Show<A>) {
		return S.show(a);
	}
}

private class ShowInt implements Show<Int> {
	public inline function new() {}

	public static inline function instance1()
		return new ShowInt();

	public static final instance3 = new ShowInt();

	public inline function show(a:Int):String {
		return "" + a;
	}
}

class Issue7274 {
	@:js('
		issues_Issue7274.use("" + 1);
	')
	static function testInlineFunction() {
		use(ShowApi.show(1, ShowInt.instance1()));
	}

	@:js('
		issues_Issue7274.use("" + 1);
	')
	static function testFinal() {
		use(ShowApi.show(1, ShowInt.instance3));
	}

	@:js('
		issues_Issue7274.use("" + 1);
	')
	static function testNew() {
		use(ShowApi.show(1, new ShowInt()));
	}

	@:pure(false)
	static function use(s:String) {}
}
