package unit.issues;

class Issue6175 extends unit.Test {
    function test() {
        eq(Method.GET, Method.test());
    }
}

@:enum abstract Method(String) {
    var GET = 'GET';
	public static function test() return Method.GET;
}