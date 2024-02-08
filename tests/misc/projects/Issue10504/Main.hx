class Main {
	static function main() {
		var v = { a : 1, foo : null };
		$type(v);
		parse1(v);
		$type(v);
		parse2(v);
	}

	extern static function parse1( v : { a : Int, ?b:Int } ):Void;
	extern static function parse2( v : { a : Int, ?b:String } ):Void;
}