enum IndependentEnum {
	Constructor(s:String);
}

class MakeDependency {
	static function f() {
		Dependency.get();
	}
}
