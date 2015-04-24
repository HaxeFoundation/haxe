package unit.issues;

// they don't allow this insanity
#if (!java && !cs)

private class Extern {

	@:keep
	static public function mytest(a:Dynamic) {
		return a;
	}

    @:overload( function (a:Int):Dynamic {})
	@:extern
    inline public static function test(a:String):Dynamic {
        return mytest(a);
    }
}

#end

class Issue3846 extends Test {
	function test() {
		#if (!java && !cs)
        eq("coucou", Extern.test("coucou"));
        eq(1, Extern.test(1));
		#end
	}
}