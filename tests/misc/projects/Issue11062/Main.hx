class Main {
    static function main() {
        var foo = new Foo();

        switch foo {
            case { foo: 123 }:
                trace('yes!');
            case { foo: 321 }:
                trace('yes!');
            default:
        }
    }
}

class Foo {
    public var foo:Int = 5;

    public var bar(get, never):Int;
    function get_bar() {
        trace('called');
        return 4;
    }

    public function new() {}
}