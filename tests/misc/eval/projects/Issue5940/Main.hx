class Main {
    static function main() {
        trace(Color.foo);
    }
}

abstract Color(Int) {
    public var foo(get, never):Int;
    public function get_foo() return 0;
}