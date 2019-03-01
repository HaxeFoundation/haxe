package unit.issues;

class Issue5384 extends unit.Test {
	function test() {
        var a:Dynamic = true;
        a = "string";

        var b:Dynamic = if (myFunc()) true else "string";

        var c:Dynamic = myFunc() ? true : "string";

        var d:Dynamic = myFunc() ? myFunc() : "string";

        var e:Dynamic = try myFunc() catch(e:Dynamic) "string";
		noAssert();
	}

    static function myFunc():Bool {
        return true;
    }
}