package issues;

class Issue11175 {
	@:js('
		throw haxe_Exception.thrown("foo");
	')
	@:analyzer(ignore)
	static function test() {
		throw "foo";
	}
}