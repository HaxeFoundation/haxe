package issues;

class Issue8128 {
	static var tmp:Any;
	@:js('
		issues_Issue8128.tmp = function() {
			return;
		};
	')
	static function test() {
		tmp = () -> {};
	}

	public function new() {}
}