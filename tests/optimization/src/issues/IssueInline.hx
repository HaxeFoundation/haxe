package issues;

import TestJs.use;

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
		var testInline = function(i) {return i + 1;};
		TestJs.use(4);
		TestJs.use(testInline(3));
		testInline(3);
	')
	static function test2() {
		function testInline(i:Int) {
			return i + 1;
		}
		use(inline testInline(3));
		inline testInline(3);
		use(testInline(3));
		testInline(3);
	}

	@:js('
		TestJs.use(4);
		TestJs.use(4);
	')
	static function test3() {
		inline function testInline(i:Int) {
			return i + 1;
		}
		use(inline testInline(3));
		inline testInline(3);
		use(testInline(3));
		testInline(3);
	}

	@:pure(false)
	static function testInline(i:Int) {
		return i + 1;
	}
}