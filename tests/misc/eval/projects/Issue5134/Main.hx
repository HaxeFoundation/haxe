@:generic
class A<T> {
    var items:Array<T>;
    public function new() {
        items = [];
    }
    inline public function get(_index:Int) : T {
        return items[_index];
    }
}

class Test {

    var a: A<Int>;

    function new() {

    }
    static function main() {
        trace("Haxe is great!");
        a.
    }
}