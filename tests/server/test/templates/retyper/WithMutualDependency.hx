class WithMutualDependency {
	static public var value = "Hello World";

	public static function main() {
		trace(MutualDependency.get());
	}
}
