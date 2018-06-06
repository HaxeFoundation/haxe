using Main;

class Main {
    static function main() {
        0.bar();
    }

    static function bar(f:Foo)
    	f.bar();
}

abstract Foo(String) {

    inline function new(v) this = v;

    @:from static function ofInt(i:Int)
      return new Foo('$i');

    public function bar() {};
    public function baz() {};
}