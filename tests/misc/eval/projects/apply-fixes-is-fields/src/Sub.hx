class Sub extends Base {
	override public function is(other:IShape) {
		return !super.is(other);
	}
}
