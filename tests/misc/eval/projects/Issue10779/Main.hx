class Main {
	public static inline var value = getValue();

	static function main() {
		Sys.println(value);
	}

	public static inline function getValue():Int {
		return 4;
	}
}