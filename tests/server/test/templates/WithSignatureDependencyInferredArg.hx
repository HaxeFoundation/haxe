class WithSignatureDependency {
	public static function main() {}

	public static function test(d) {
		d = new Dependency();
	}
}
