class Main {
    static function main() {
        var func = inline function(a:Int, b:String, c:Float) return a;
        func(1, "foo", 1.0);
    }
}