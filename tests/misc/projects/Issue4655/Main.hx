/** Root level Test1. This comment will exist */
typedef Test1 = {
	/**test one (this comment will not exist)*/
	@:meta1
	?a: Int,

	/**test other (this comment will not exist either)*/
	@:meta2
	b: Int
}

class Main {
	static function main() {
		var t:Test1;
		var info = Macro.getMetaAndDocInfo(t);
		function println(s) {
			Sys.stderr().writeString(s + "\n");
		}
		println(info.a.doc);
		println(info.a.meta);
		println(info.b.doc);
		println(info.b.meta);
	}
}