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

	@:pure(false)
	static function testInline(i:Int) {
		return i + 1;
	}
}