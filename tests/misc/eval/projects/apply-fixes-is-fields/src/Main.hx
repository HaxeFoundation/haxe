class Main {
	var isValue = 1;

	static function main() {
		var shape:IShape = new Sub();
		var f = shape.is;
		trace(shape.is(new Base()), f(shape), Util.check(1), new User().is(1));
	}
}
