package unit.issues;

class Issue3280 extends Test {

    static function recurse( i : Int, param : String ) {
        if( i > 0 ) {
			param += iter(i, recurse.bind(_,param));
		}
		return param;
    }

    static function iter( v : Int, f) {
        return f(v-1);
    }

	function test() {
		eq("aaaa", recurse(3, "a"));
	}
}