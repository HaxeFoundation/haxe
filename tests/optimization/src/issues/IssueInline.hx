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

private class ParamClass<T> {
	public var x:T;

	public function new(x:T) {
		this.x = x;
	}

	public function test(a:T) {
		use(a);
		use(x);
	}
}

private abstract ParamAbstract<T>(T) {
	public function new(x:T) {
		this = x;
	}

	public function test(a:T) {
		use(a);
		use(this);
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
		TestJs.use(10);
		TestJs.use(1);
	')
	static function testParamClass() {
		var p = inline new ParamClass<Int>(1);
		inline p.test(10);
	}

	@:js('
		TestJs.use(10);
		TestJs.use(1);
	')
	static function testParamAbstract() {
		var p = inline new ParamAbstract<Int>(1);
		inline p.test(10);
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