enum A {
    A1(v:String);
    A2(v:B);
}
enum B {
    BB(v:Float);
}
class Test {
    public static function main () {
        var v1 = A2(BB(12));
        v1 = switch (v1) {
            case A2(v):
                switch (v) {
                    case BB(v): A2(BB(v++));
                }
            default: A1("");
        }
        trace(v1);
    }
}