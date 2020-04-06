class Main {
	public static function main() {
		Under.from({});
	}
}

abstract Under<T>(T) {
	@:from public static function from<T>(v):Under<T>
		return v;
}