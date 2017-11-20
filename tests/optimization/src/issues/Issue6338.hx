package issues;

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
		console.log("hd");
		console.log("ft");
	')
	@:analyzer(ignore)
	static function test() {
		trace(Home.ids.hd);
		trace(Home.ids.ft);
	}
}