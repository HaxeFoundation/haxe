class Base implements IShape {
	public function new() {}

	public function is(other:IShape) {
		return other == this;
	}
}
