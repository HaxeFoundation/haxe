package issues;

class Issue5477 {
	@:js('
		issues_Issue5477["use"](issues_Issue5477.pureUse(12) > 0.5?1:issues_Issue5477.pureUse(12));
	')
	static function testIssue5477() {
		var v = pureUse(12);
		use(pureUse(12) > 0.5 ? 1 : v);
	}

	@:impure
	static function use<T>(t:T) { return t; }
	@:pure
	static function pureUse<T>(t:T) { return t; }
}