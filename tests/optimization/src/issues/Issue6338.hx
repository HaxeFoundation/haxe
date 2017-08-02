package issues;

import TestJs.use;

extern class Home {
	static var ids: Ids;
}

extern class Ids {
	var hd(get, never): String;
	inline function get_hd(): String return "hd";

	var ft(get, never): String;
	inline function get_ft(): String return "ft";
}

class Issue6338 {
	@:js('
		TestJs["use"]("hd");
		TestJs["use"]("ft");
	')
	@:analyzer(ignore)
	static function test() {
		use(Home.ids.hd);
		use(Home.ids.ft);
	}
}