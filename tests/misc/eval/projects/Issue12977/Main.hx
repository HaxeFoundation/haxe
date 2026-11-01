class Main {
	static function f<A:C, B:C, C>(a:A, b:B, c:C):Void {}

	static function main() {
		#if case_violate_a
		f(1, "ok", "str");
		#end
		#if case_violate_b
		f("ok", 1, "str");
		#end
	}
}
