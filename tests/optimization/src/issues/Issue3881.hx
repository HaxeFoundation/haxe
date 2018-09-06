package issues;

private class A implements B {

    public function new () {}

    public inline function plus (a:Int,b:Int):Int {
        return a + b;
    }
}

private interface B {
    public function plus (a:Int,b:Int):Int;
}

class Issue3881 {
    public inline static function plusFoo (x:B) {
        return x.plus(1,1);
    }

	@:js('issues_Issue3881.call(2);')
    public static function test() {
        var a = new A();
        call(plusFoo(a));
	}

	@:pure(false) static function call(d) { }
}