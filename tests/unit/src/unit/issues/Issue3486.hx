package unit.issues;

class Issue3486 extends Test {
	function test() {
        var l = new List();
        flatMap(l);
	}

    static public function flatMap<B>(a:List<B>) {
        for (x in a) {}
    }
}