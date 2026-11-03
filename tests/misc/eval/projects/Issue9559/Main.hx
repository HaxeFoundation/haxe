class Main {
	static function main() {
		function f<T:String>(t:T) {}

		f("foo");
		f(12);
	}
}
