typedef S = {a:Int, ?b:Int};

class Main {
    static function f1(a:S):Void {}
    static function f2<T:S>(a:T):Void {}

    static function main() {
        f1({a: 1}); // works
        f2({a: 1}); // Constraint check failure: { a : Int } should be { ?b : Null<Int>, a : Int }
        f2(({a: 1} : S)); // works
    }
}
