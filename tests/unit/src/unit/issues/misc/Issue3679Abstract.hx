package unit.issues.misc;

abstract Issue3679Abstract(Int) {
	public function new(i) this = i;
	function get() return this;
	static public function extract(a:Issue3679Abstract) return a.get();
}