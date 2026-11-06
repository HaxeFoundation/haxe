class User extends LibBase {
	override public function is(x:Int) {
		return !super.is(x);
	}
}
