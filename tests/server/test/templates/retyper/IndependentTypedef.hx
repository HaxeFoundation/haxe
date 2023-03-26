typedef IndependentTypedef = String;

class MakeDependency {
	static function f() {
		Dependency.get();
	}
}
