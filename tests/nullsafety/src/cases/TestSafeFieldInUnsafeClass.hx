package cases;

@:build(Validator.checkFields())
class TestSafeFieldInUnsafeClass {
	@:nullSafety
	static public function shouldFail(?a:String) {
		Validator.shouldFail(var s:String = a);
	}
}