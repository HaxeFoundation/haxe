abstract A(Int) from Int {
	public var x(get,set):Int;

	function get_x() return this;

	inline function set_x(value) return this = value;

	public function modify() {
		x += 3;
	}
}

class Main {
	static function main() {}
}