package issues;

import TestJs.use;

private class Point {
	public var x:Int;
	public var y:Int;

	public function new(x:Int, y:Int) {
		this.x = x;
		this.y = y;
	}
}

private class InlinePoint {
	public var x:Int;
	public var y:Int;

	inline public function new(x:Int, y:Int) {
		this.x = x;
		this.y = y;
	}
}

private abstract InlineAbstract(Int) {
	public function new(x:Int) {
		this = x;
	}

	public function twice() {
		return this * 2;
	}
}

class IssueInline {
	@:js('
		TestJs.use(4);
		TestJs.use(issues_IssueInline.testInline(3));
		issues_IssueInline.testInline(3);
	')
	static function test() {
		use(inline testInline(3));
		inline testInline(3);
		use(testInline(3));
		testInline(3);
	}

	@:js('
		TestJs.use(1);
		TestJs.use(2);
	')
	static function testCtor1() {
		var x = inline new Point(1, 2);
		use(x.x);
		use(x.y);
	}

	@:js('
		TestJs.use(1);
		TestJs.use(2);
	')
	static function testCtor2() {
		var x = inline new InlinePoint(1, 2);
		use(x.x);
		use(x.y);
	}

	@:js('
		TestJs.use(24);
	')
	static function testAbstract() {
		var a = inline new InlineAbstract(12);
		use(inline a.twice());
	}

	@:pure(false)
	static function testInline(i:Int) {
		return i + 1;
	}
}