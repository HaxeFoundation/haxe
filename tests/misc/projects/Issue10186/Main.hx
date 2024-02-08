abstract Main(Dynamic) {
	static function main() {}
	var bar(get, never):Int;
	public function foo(?v = bar) {}
	function get_bar() return 1;
}