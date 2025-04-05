extern class C {
    @:overload(function<T:Int>(t:T):Void {})
    static function f():Void;
}

class Main {
    static function main() {
        C.f("hi");
        C.f({});
    }
}