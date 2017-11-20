class Main {
    static function main() {
        f("Haxe is great!") + "abc"; // This must produce a error!
    }
    static function f(s:String) : Void trace(s);
}