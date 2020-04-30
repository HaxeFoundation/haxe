package issues;

class Issue8770 {
	@:pure(false) static var testStatic:String;
	static var pureStatic:String;

	@:pure(false) var testInstance:String;
	var pureInstance:String;

	@:js('
		var m_h;
		m_h = { };
	')
	@:analyzer(ignore)
	static function test() {
		var m = new Map<Int,Bool>();
	}

	public function new() {}
}