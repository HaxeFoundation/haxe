package unit.issues;

private abstract Error(String) {
    public function new(message:String) {
        this = message;
    }
}

class Issue2859 extends Test {
	function test() {
        try {
            throw new Error("hello");
        } catch (e:Error) {
			unit.TestType.typedAs(e, (null:Error));
            func(e);
        }
    }

    static function func(e:Error) { }
}