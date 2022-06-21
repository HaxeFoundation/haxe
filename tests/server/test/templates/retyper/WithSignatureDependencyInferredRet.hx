class WithSignatureDependency {
	public static function main() {}

	public static function test() {
		return new Dependency();
	}
}
