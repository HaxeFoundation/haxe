interface I {
    function f(a:Int):Void;
}

class C implements I {
    public function f() {} // missing `a` argument
}
