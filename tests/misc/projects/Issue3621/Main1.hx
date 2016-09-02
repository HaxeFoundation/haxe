@:native("lol")
@:enum extern abstract E(String) {
    var A;
    var B;
    var C;
}

class Main1 {
    static var a:E;
    static function main() {
        switch (a) {
            case A: trace("hello");
            case B: trace("yo");
        }
    }
}