class TestStrings {
	static public function reusing(module:String) {
		return "reusing " + module;
	}

	static public function skipping(module:String) {
		return "skipping " + module;
	}

	static public function skippingDep(module:String, moduleDep:String) {
		return 'skipping $module($moduleDep)';
	}

	static public function notCachedModified(module:String) {
		return module + " not cached (modified)";
	}
}