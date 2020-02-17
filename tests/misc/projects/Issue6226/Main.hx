class Main {
	@:deprecated
	static var b(get, never):Int;
	static inline function get_b() return 1;

	public static function main() {
		b;
	}
}
