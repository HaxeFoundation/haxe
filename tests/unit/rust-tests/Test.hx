class Test {
	public static function main() {
		Sys.println("Hello, world!");
		isCool(78.9);
	}
	static function isCool<T>(v:T):Bool {
		return true;
	}
}