package unit.issues;

class Issue9757 extends unit.Test {
#if sys
	static var system:Null<String>;

	static public function __init__() {
		system = Sys.systemName();
	}
	function test() {
		t(system != null);
	}
#end
}