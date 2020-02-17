package cases;

@:nullSafety
abstract TestAbstract(Null<Float>) from Null<Float> to Null<Float> {
	public inline function testThis() {
		Validator.shouldFail(var f:Float = this);
	}
}