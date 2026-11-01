class Test {
    public function new() {}
}

class Main {
    public static function main() {
        new Generic<Test>();
    }
}

@:generic
class Generic<T:({function new():Void;})> {
    var t:T;

    public function new() {
        t = new T();
    }
}