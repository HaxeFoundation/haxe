class Main {
	final prev:Null<A> = null;
	var nextChar = TriState.UNKNOWN;

	function new() {
		final prevChar = @:nullSafety(Off) prev.get();
		try {} catch (ex:String) nextChar = FALSE;
	}

	static function main() {}
}

abstract A(Bool) {
	inline public function get():Int return _get();
	function _get():Int return 1;
}

enum abstract TriState(Null<Bool>) from Null<Bool> to Null<Bool> {
	final TRUE = true;
	final FALSE = false;
	final UNKNOWN = null;
}
