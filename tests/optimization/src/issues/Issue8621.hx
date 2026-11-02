package issues;

import TestJs.use;

private abstract A(Int) from Int {}

class Issue8621 {
	@:js('
		var m_h = { };
		m_h[0] = 1;
		TestJs.use(m_h[0]);
	')
	static function test() {
		var m = new Map<A, Int>();
		m.set(0, 1);
		use(m.get(0));
	}
}
