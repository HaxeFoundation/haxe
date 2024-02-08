class Main {
	static function main() {
		(null : Foo).bar;
	}
}

abstract Foo(String) from String {
	public var bar(get, never):String;

	inline function get_bar() {
		return this;
	}
}