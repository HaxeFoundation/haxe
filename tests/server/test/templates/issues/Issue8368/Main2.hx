class Main {
	public static function main() {
		buildSimplest();
	}

	macro static function buildSimplest() {
		var cb = new MyMacro(null);
		cb.build();
		return macro {}
	}
}
