abstract AbstractWithDependency(String) {
	public static function notMain() {
		trace(Dependency.get());
	}
}
