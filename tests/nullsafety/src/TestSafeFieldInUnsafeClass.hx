@:build(Validator.checkFields())
class TestUnsafeFieldInSafeClass {
	@:nullSafety
	static public function shouldFail(?a:String) {
		Validator.shouldFail(var s:String = a);
	}
}