package unit.issues;

class Issue3344 extends unit.Test {

    static function hello() {
		return "ok";
    }

	function test() {
        eq("ok", Issue3344.hello());
        var Issue3344 = 10;
		eq(10, Issue3344);
	}
}